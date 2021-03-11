{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (foldrM)
import Data.Generics.Labels ()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import HieBin (HieFileResult)
import qualified HieBin
import qualified HieTypes
import NameCache (NameCache)
import qualified NameCache
import qualified System.Directory as Directory
import qualified System.Directory.Recursive as Directory.Recursive
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
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
      Text.intercalate
        "\t"
        [ Text.pack filePath,
          (Text.pack . HieTypes.hie_hs_file . HieBin.hie_file_result) hieFileResult,
          (Text.pack . show . HieBin.hie_file_result_version) hieFileResult,
          (Text.Encoding.decodeUtf8 . HieBin.hie_file_result_ghc_version) hieFileResult
        ]

writeVersionReport :: FilePath -> [(FilePath, HieFileResult)] -> IO ()
writeVersionReport reportsDir hieFileResults = do
  let versionReportPath = reportsDir <> "version-report.tsv"
  putStrLn $ "Writing " <> versionReportPath
  ByteString.writeFile versionReportPath $ Text.Encoding.encodeUtf8 $ makeVersionReport hieFileResults

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
  writeVersionReport reportsDir hieFileResults
  checkHieVersions hieFileResults
