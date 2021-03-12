{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Avail
import Control.Monad (when)
import qualified Data.Array as Array
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (foldrM)
import Data.Generics.Labels ()
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified FastString
import HieBin (HieFileResult)
import qualified HieBin
import qualified HieTypes
import qualified Module
import NameCache (NameCache)
import qualified NameCache
import qualified System.Directory as Directory
import qualified System.Directory.Recursive as Directory.Recursive
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import qualified UniqSet
import qualified UniqSupply

loadHieFiles :: NameCache -> [FilePath] -> IO (NameCache, [(FilePath, HieFileResult)])
loadHieFiles initialNameCache = foldrM go (initialNameCache, [])
  where
    go filePath (inputNameCache, hieFileResults) =
      do
        (hieFileResult, outputNameCache) <- HieBin.readHieFile inputNameCache filePath
        return (outputNameCache, (filePath, hieFileResult) : hieFileResults)

findHieFiles :: FilePath -> IO [FilePath]
findHieFiles dir = do
  allFiles <- Directory.Recursive.getFilesRecursive dir
  pure $ filter (\path -> FilePath.takeExtension path == ".hie") allFiles

handleInputPath :: FilePath -> IO [FilePath]
handleInputPath path = do
  isDir <- Directory.doesDirectoryExist path
  if isDir
    then do
      putStrLn $ "Finding all .hie files in directory: " <> path
      findHieFiles path
    else do
      putStrLn $ "Loading as list of .hie files: " <> path
      bytes <- ByteString.readFile path
      let text = Text.Encoding.decodeUtf8 bytes
      pure $
        fmap Text.unpack $
          filter (not . Text.null) $
            Text.lines text

makeVersionReport :: [(FilePath, HieFileResult)] -> Text
makeVersionReport = Text.unlines . (headerLine :) . fmap makeLine
  where
    headerLine =
      Text.intercalate
        "\t"
        [ "HIE File",
          "Haskell Source File",
          "HIE File Version",
          "GHC Version"
        ]
    makeLine (filePath, hieFileResult) =
      let hieFile = HieBin.hie_file_result hieFileResult
       in Text.intercalate
            "\t"
            [ Text.pack filePath,
              (Text.pack . HieTypes.hie_hs_file) hieFile,
              (Text.pack . show . HieBin.hie_file_result_version) hieFileResult,
              (Text.Encoding.decodeUtf8 . HieBin.hie_file_result_ghc_version) hieFileResult
            ]

makeStatsReport :: [(FilePath, HieFileResult)] -> Text
makeStatsReport = Text.unlines . (headerLine :) . fmap makeLine
  where
    headerLine =
      Text.intercalate
        "\t"
        [ "HIE File",
          "Haskell Source File",
          "AST filepath",
          "Module UnitId",
          "Module Name",
          "Types used",
          "Exports",
          "AST filepath count",
          "AST top-level children"
        ]
    makeLine (filePath, hieFileResult) =
      let hieFile = HieBin.hie_file_result hieFileResult
          hieModule = HieTypes.hie_module hieFile
       in Text.intercalate
            "\t"
            [ Text.pack filePath,
              (Text.pack . HieTypes.hie_hs_file) hieFile,
              getFirstAstFile hieFile,
              (makeUnitIdText . Module.moduleUnitId) hieModule,
              (Text.pack . Module.moduleNameString . Module.moduleName) hieModule,
              (Text.pack . show . (\(high, low) -> abs (high - low) + 1) . Array.bounds . HieTypes.hie_types) hieFile,
              (Text.pack . show . UniqSet.sizeUniqSet . Avail.availsToNameSet . HieTypes.hie_exports) hieFile,
              (Text.pack . show . Map.size . HieTypes.getAsts . HieTypes.hie_asts) hieFile,
              (Text.pack . show . initialAstChildrenCount) hieFile
            ]

-- | The Map always seems to have 1 or 0 elements.
getFirstAstFile :: HieTypes.HieFile -> Text
getFirstAstFile hieFile =
  let astMap = (HieTypes.getAsts . HieTypes.hie_asts) hieFile
   in case Map.assocs astMap of
        [] -> "No ASTs"
        (astPath, _) : _ -> (Text.pack . FastString.unpackFS) astPath

initialAstChildrenCount :: HieTypes.HieFile -> Int
initialAstChildrenCount hieFile = maybe 0 (length . HieTypes.nodeChildren) (getMaybeAst hieFile)

getMaybeAst :: HieTypes.HieFile -> Maybe (HieTypes.HieAST HieTypes.TypeIndex)
getMaybeAst hieFile =
  let astMap = (HieTypes.getAsts . HieTypes.hie_asts) hieFile
   in case Map.assocs astMap of
        [] -> Nothing
        (_, ast) : _ -> Just ast

makeUnitIdText :: Module.UnitId -> Text
makeUnitIdText (Module.IndefiniteUnitId _) = "IndefiniteUnitId"
makeUnitIdText (Module.DefiniteUnitId defUnitId) = (Text.pack . FastString.unpackFS . Module.installedUnitIdFS . Module.unDefUnitId) defUnitId

writeReport :: FilePath -> ([(FilePath, HieFileResult)] -> Text) -> [(FilePath, HieFileResult)] -> IO ()
writeReport reportPath makeReport hieFileResults = do
  putStrLn $ "Writing " <> reportPath
  ByteString.writeFile reportPath $ Text.Encoding.encodeUtf8 $ makeReport hieFileResults

-- | Index by HIE version (Integer) and GHC version (ByteString)
makeVersionMap :: [(FilePath, HieFileResult)] -> Map (Integer, ByteString) [(FilePath, HieFileResult)]
makeVersionMap = Map.unionsWith (<>) . fmap go
  where
    go pair@(_, hieFileResult) =
      Map.singleton
        (HieBin.hie_file_result_version hieFileResult, HieBin.hie_file_result_ghc_version hieFileResult)
        [pair]

checkHieVersions :: [(FilePath, HieFileResult)] -> IO ()
checkHieVersions hieFileResults = do
  let versionMap = makeVersionMap hieFileResults
      versionMapKeys = Map.keys versionMap
  case versionMapKeys of
    [] -> fail "No .hie files found"
    [(hieVersion, _ghcVersion)] -> do
      when (HieTypes.hieVersion /= hieVersion) do
        fail $ "Our HIE version: " <> show HieTypes.hieVersion <> " does not match .hie file version: " <> show hieVersion
    _ : _ : _ -> do
      putStrLn "Multiple versions of HIE/GHC found. See version report for details."
      mapM_ (\(hieVersion, ghcVersion) -> putStrLn $ show hieVersion <> " / " <> (Text.unpack . Text.Encoding.decodeUtf8) ghcVersion) versionMapKeys

processASTs :: [(FilePath, HieFileResult)] -> IO ()
processASTs hieFileResults = do
  let astFilePathSet = foldr buildAstFilePathSet Set.empty hieFileResults
  putStrLn $ "AST key count: " <> (show . Set.size) astFilePathSet

  when False do
    mapM_ (putStrLn . FastString.unpackFS) astFilePathSet

  let topLevelNodeInfos = concatMap (maybe [] (pure . HieTypes.nodeInfo) . getMaybeAst . HieBin.hie_file_result . snd) hieFileResults
  let topLevelNodePairs = foldr buildNodeConstructorNodeTypePairs Set.empty topLevelNodeInfos
  putStrLn $ "Top-level node constructor/type pair count: " <> (show . Set.size) topLevelNodePairs
  mapM_ (\(ctr, typ) -> putStrLn $ FastString.unpackFS ctr <> " / " <> FastString.unpackFS typ) topLevelNodePairs

  where
    buildAstFilePathSet (_hiePath, hieFileResult) inputSet =
      let astMap = (HieTypes.getAsts . HieTypes.hie_asts . HieBin.hie_file_result) hieFileResult
      in Set.union inputSet (Map.keysSet astMap)

    buildNodeConstructorNodeTypePairs nodeInfo inputSet =
      let pairSet = HieTypes.nodeAnnotations nodeInfo
      in Set.union inputSet pairSet

main :: IO ()
main = do
  args <- Environment.getArgs
  inputPath <-
    case args of
      [arg] -> pure arg
      [] -> fail "Required argument: EITHER directory with any .hie files in subdirectories OR a file where each line is an absolute path to an .hie file"
      _ : _ : _ -> fail "Only pass one path argument"

  hieFilePaths <- handleInputPath inputPath
  putStrLn $ ".hie files found: " <> (show . length) hieFilePaths

  putStrLn "Loading .hie files"
  uniqSupply <- UniqSupply.mkSplitUniqSupply 'Q'
  let initialNameCache = NameCache.initNameCache uniqSupply []
  (_finalNameCache, hieFileResults) <- loadHieFiles initialNameCache hieFilePaths
  putStrLn $ ".hie files loaded: " <> (show . length) hieFileResults

  let reportsDir = "reports/"
  Directory.createDirectoryIfMissing True reportsDir
  writeReport (reportsDir <> "version-report.tsv") makeVersionReport hieFileResults
  checkHieVersions hieFileResults

  writeReport (reportsDir <> "stats-report.tsv") makeStatsReport hieFileResults

  processASTs hieFileResults
