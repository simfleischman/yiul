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
import qualified System.Directory as Directory
import qualified System.Directory.Recursive as Directory.Recursive
import qualified System.FilePath as FilePath
import qualified UniqSupply

topLevelLoadHieFiles :: [FilePath] -> IO [(FilePath, HieFileResult)]
topLevelLoadHieFiles hieFilePaths = do
  putStrLn "Loading .hie files"
  uniqSupply <- UniqSupply.mkSplitUniqSupply 'Q'
  let initialNameCache = NameCache.initNameCache uniqSupply []
  (_finalNameCache, hieFileResults) <- loadHieFiles initialNameCache hieFilePaths
  putStrLn $ ".hie files loaded: " <> (show . length) hieFileResults
  pure hieFileResults

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
