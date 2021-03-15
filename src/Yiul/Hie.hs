{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Yiul.Hie where

import qualified Data.ByteString as ByteString
import Data.Foldable (foldrM)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import HieBin (HieFileResult)
import qualified HieBin
import NameCache (NameCache)
import qualified NameCache
import qualified System.Directory.Recursive as Directory.Recursive
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified UniqSupply

topLevelLoadHieFiles :: FilePath -> [FilePath] -> IO [(FilePath, HieFileResult)]
topLevelLoadHieFiles projectDir hieFilePaths = do
  putStrLn "Loading .hie files"
  uniqSupply <- UniqSupply.mkSplitUniqSupply 'Q'
  let initialNameCache = NameCache.initNameCache uniqSupply []
  (_finalNameCache, hieFileResults) <- loadHieFiles initialNameCache projectDir hieFilePaths
  putStrLn $ ".hie files loaded: " <> (show . length) hieFileResults
  pure hieFileResults

loadHieFiles :: NameCache -> FilePath -> [FilePath] -> IO (NameCache, [(FilePath, HieFileResult)])
loadHieFiles initialNameCache projectDir = foldrM go (initialNameCache, [])
  where
    go filePath (inputNameCache, hieFileResults) =
      do
        (hieFileResult, outputNameCache) <- HieBin.readHieFile inputNameCache (projectDir </> filePath)
        return (outputNameCache, (filePath, hieFileResult) : hieFileResults)

findHieFiles :: FilePath -> IO [FilePath]
findHieFiles dir = do
  allFiles <- Directory.Recursive.getFilesRecursive dir
  pure $
    FilePath.makeRelative dir
      <$> filter (\path -> FilePath.takeExtension path == ".hie") allFiles

loadHieFileList :: FilePath -> FilePath -> IO [FilePath]
loadHieFileList projectDir path = do
  bytes <- ByteString.readFile path
  let text = Text.Encoding.decodeUtf8 bytes
      tryMakeRelative file =
        if FilePath.isAbsolute file
          then FilePath.makeRelative projectDir file
          else file
  pure $
    fmap (tryMakeRelative . Text.unpack) $
      filter (not . Text.null) $
        Text.lines text
