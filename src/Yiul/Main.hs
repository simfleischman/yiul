{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Yiul.Main where

import qualified Avail
import Control.Monad (when)
import qualified Data.Array as Array
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
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
import qualified Name
import NameCache (NameCache)
import qualified NameCache
import qualified SrcLoc
import qualified System.Directory as Directory
import qualified System.Directory.Recursive as Directory.Recursive
import qualified System.Environment as Environment
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified UniqSet
import qualified UniqSupply
import qualified Yiul.GhcPkg

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

  let topLevelAsts = concatMap (maybe [] pure . getMaybeAst . HieBin.hie_file_result . snd) hieFileResults
      topLevelNodeInfos = HieTypes.nodeInfo <$> topLevelAsts
  let topLevelNodePairs = foldr buildNodeConstructorNodeTypePairCount Map.empty topLevelNodeInfos
  putStrLn $ "Top-level node constructor/type pair count: " <> (show . Map.size) topLevelNodePairs
  mapM_
    (\((ctr, typ), ct) -> putStrLn $ FastString.unpackFS ctr <> " / " <> FastString.unpackFS typ <> " : " <> show ct)
    (Map.assocs topLevelNodePairs)

  let subModuleTopLevelNodeInfos = HieTypes.nodeInfo <$> concatMap HieTypes.nodeChildren topLevelAsts
      subModuleTopLevelNodeInfoMap = foldr buildNodeConstructorNodeTypePairCount Map.empty subModuleTopLevelNodeInfos
  putStrLn $ "Sub-module node constructor/type pair count: " <> (show . Map.size) subModuleTopLevelNodeInfoMap
  mapM_
    (\((ctr, typ), ct) -> putStrLn $ FastString.unpackFS ctr <> " / " <> FastString.unpackFS typ <> " : " <> show ct)
    (Map.assocs subModuleTopLevelNodeInfoMap)
  where
    buildAstFilePathSet (_hiePath, hieFileResult) inputSet =
      let astMap = (HieTypes.getAsts . HieTypes.hie_asts . HieBin.hie_file_result) hieFileResult
       in Set.union inputSet (Map.keysSet astMap)

    buildNodeConstructorNodeTypePairCount nodeInfo inputMap =
      let maps = Map.fromSet (const (1 :: Int)) (HieTypes.nodeAnnotations nodeInfo)
       in Map.unionsWith (+) [inputMap, maps]

makeAstStatsReport :: [(FilePath, HieFileResult)] -> Text
makeAstStatsReport = Text.unlines . (headerLine :) . concatMap handlePair
  where
    headerLine =
      Text.intercalate
        "\t"
        [ "Span",
          "Node Annotations",
          "Modules",
          "Node Identifiers"
        ]
    handlePair (filePath, hieFileResult) =
      case (getMaybeAst . HieBin.hie_file_result) hieFileResult of
        Nothing -> []
        Just ast -> makeAstLines filePath ast
    makeAstLines filePath ast =
      let nodeInfo = HieTypes.nodeInfo ast
          nodeAnnotationsText =
            Text.intercalate ", "
              . fmap (\(c, t) -> (Text.pack . FastString.unpackFS) c <> "/" <> (Text.pack . FastString.unpackFS) t)
              . Set.toList
              . HieTypes.nodeAnnotations
              $ nodeInfo
          nodeIdentifiersText =
            Text.intercalate "; "
              . fmap (\(identifier, details) -> identifierToText identifier <> " " <> (Text.pack . show . Set.toList . HieTypes.identInfo) details)
              . Map.assocs
              $ HieTypes.nodeIdentifiers nodeInfo
          modulesText =
            Text.intercalate "; "
              . fmap identifierToModuleText
              . Map.keys
              $ HieTypes.nodeIdentifiers nodeInfo
          currentLine =
            Text.intercalate
              "\t"
              [ (Text.pack . show . HieTypes.nodeSpan) ast,
                nodeAnnotationsText,
                modulesText,
                nodeIdentifiersText
              ]
          nextLines = concatMap (makeAstLines filePath) (HieTypes.nodeChildren ast)
       in currentLine : nextLines

identifierToText :: Either Module.ModuleName Name.Name -> Text
identifierToText (Left moduleName) = (Text.pack . Module.moduleNameString) moduleName
identifierToText (Right name) = (Text.pack . Name.nameStableString) name

identifierToModuleText :: Either Module.ModuleName Name.Name -> Text
identifierToModuleText (Left moduleName) =
  "ModuleName: " <> (Text.pack . Module.moduleNameString) moduleName
identifierToModuleText (Right name) =
  case Name.nameModule_maybe name of
    Nothing -> "NameNoModule: " <> (Text.pack . Name.nameStableString) name
    Just (Module.Module unitId moduleName) -> "UnitId: " <> makeUnitIdText unitId <> ", module: " <> (Text.pack . Module.moduleNameString) moduleName

realSrcSpanToText :: SrcLoc.RealSrcSpan -> Text
realSrcSpanToText srcSpan =
  (realSrcLocToText . SrcLoc.realSrcSpanStart) srcSpan
    <> "-"
    <> (Text.pack . show . SrcLoc.srcLocLine . SrcLoc.realSrcSpanEnd) srcSpan
    <> ":"
    <> (Text.pack . show . SrcLoc.srcLocCol . SrcLoc.realSrcSpanEnd) srcSpan

realSrcLocToText :: SrcLoc.RealSrcLoc -> Text
realSrcLocToText loc =
  (Text.pack . FastString.unpackFS . SrcLoc.srcLocFile) loc
    <> ":"
    <> (Text.pack . show . SrcLoc.srcLocLine) loc
    <> ":"
    <> (Text.pack . show . SrcLoc.srcLocCol) loc

srcLocToText :: SrcLoc.SrcLoc -> Text
srcLocToText (SrcLoc.RealSrcLoc loc) =
  (Text.pack . FastString.unpackFS . SrcLoc.srcLocFile) loc
    <> ":"
    <> (Text.pack . show . SrcLoc.srcLocLine) loc
    <> ":"
    <> (Text.pack . show . SrcLoc.srcLocCol) loc
srcLocToText (SrcLoc.UnhelpfulLoc fastString) = "UnhelpfulLoc:" <> (Text.pack . FastString.unpackFS) fastString

main :: IO ()
main = do
  args <- Environment.getArgs
  inputPath <-
    case args of
      [arg] -> pure arg
      [] -> fail "Required argument: EITHER directory with any .hie files in subdirectories OR a file where each line is an absolute path to an .hie file"
      _ : _ : _ -> fail "Only pass one path argument"

  putStrLn $ "Loading ghc-pkg dump output: " <> inputPath
  bytes <- ByteString.Lazy.readFile inputPath
  case Yiul.GhcPkg.parsePackages bytes of
    Left errs -> do
      mapM_ putStrLn errs
      fail "See above errors parsing ghc-pkg dump output"
    Right results -> do
      putStrLn $ "Loaded " <> (show . length) results <> " package infos"

  when False do
    hieFilePaths <- handleInputPath inputPath
    putStrLn $ ".hie files found: " <> (show . length) hieFilePaths

    putStrLn "Loading .hie files"
    uniqSupply <- UniqSupply.mkSplitUniqSupply 'Q'
    let initialNameCache = NameCache.initNameCache uniqSupply []
    (_finalNameCache, hieFileResults) <- loadHieFiles initialNameCache hieFilePaths
    putStrLn $ ".hie files loaded: " <> (show . length) hieFileResults

    let reportsDir = "reports"
    Directory.createDirectoryIfMissing True reportsDir
    writeReport (reportsDir </> "version-report.tsv") makeVersionReport hieFileResults
    checkHieVersions hieFileResults

    writeReport (reportsDir <> "stats-report.tsv") makeStatsReport hieFileResults

    processASTs hieFileResults
    writeReport (reportsDir <> "ast-report.tsv") makeAstStatsReport hieFileResults
