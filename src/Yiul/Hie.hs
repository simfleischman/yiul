{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified System.FilePath as FilePath
import System.IO (hPutStrLn, stderr)
import qualified UniqSupply
import Yiul.Const
import Yiul.Directory (removeDotDirectories)

topLevelLoadHieFiles :: ProjectDir -> [HieFilePath] -> IO [(HieFilePath, HieFileResult)]
topLevelLoadHieFiles projectDir hieFilePaths = do
  hPutStrLn stderr "Loading .hie files"
  uniqSupply <- UniqSupply.mkSplitUniqSupply 'Q'
  let initialNameCache = NameCache.initNameCache uniqSupply []
  (_finalNameCache, hieFileResults) <- loadHieFiles initialNameCache projectDir hieFilePaths
  hPutStrLn stderr $ ".hie files loaded: " <> (show . length) hieFileResults
  pure hieFileResults

loadHieFiles :: NameCache -> ProjectDir -> [HieFilePath] -> IO (NameCache, [(HieFilePath, HieFileResult)])
loadHieFiles initialNameCache projectDir = foldrM go (initialNameCache, [])
  where
    go filePath (inputNameCache, hieFileResults) =
      do
        (hieFileResult, outputNameCache) <- HieBin.readHieFile inputNameCache (unConst (projectDir </> filePath))
        return (outputNameCache, (filePath, hieFileResult) : hieFileResults)

findHieFiles :: ProjectDir -> IO [HieFilePath]
findHieFiles dir = do
  allFiles <-
    fmap (mkConst @HieFilePath . removeDotDirectories)
      <$> Directory.Recursive.getFilesRecursive (unConst dir)
  let filteredFiles = filter (\path -> FilePath.takeExtension (unConst path) == ".hie") allFiles
  pure $ fmap (makeRelativeFilePath dir) filteredFiles

loadHieFileList :: ProjectDir -> HieFileListPath -> IO [HieFilePath]
loadHieFileList projectDir path = do
  bytes <- ByteString.readFile (unConst path)
  let text = Text.Encoding.decodeUtf8 bytes
      tryMakeRelative file =
        if FilePath.isAbsolute (unConst file)
          then makeRelativeFilePath projectDir file
          else file
  pure $
    fmap (tryMakeRelative . mkConst . removeDotDirectories . Text.unpack) $
      filter (not . Text.null) $
        Text.lines text
