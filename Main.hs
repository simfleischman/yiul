module Main where

import Control.Lens ((^.))
import Data.Generics.Labels ()
import qualified Avail
import qualified Control.Lens as Lens
import qualified Control.Monad.State.Strict as MonadState
import qualified Data.Array as Array
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified FastString
import qualified FieldLabel
import qualified GHC
import qualified GHC.Paths
import qualified HieBin
import qualified HieTypes
import qualified HieUtils
import qualified Module
import qualified Name
import qualified NameCache
import qualified Outputable
import qualified UniqSupply
import NameCache (NameCache)
import HieBin (HieFileResult)
import Data.Foldable (foldrM)
import qualified System.Directory as Directory
import qualified System.Directory.Recursive as Directory.Recursive
import qualified System.FilePath as FilePath
import qualified System.Environment as Environment
import qualified System.IO
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding

loadHieFiles :: NameCache -> [FilePath] -> IO (NameCache, [HieFileResult])
loadHieFiles initialNameCache = foldrM go (initialNameCache, [])
  where
  go filePath (inputNameCache, hieFileResults) =
    do
      (hieFileResult, outputNameCache) <- HieBin.readHieFile inputNameCache filePath
      return (outputNameCache, hieFileResult : hieFileResults)

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
      bytes <- Data.ByteString.readFile path
      let text = Data.Text.Encoding.decodeUtf8 bytes
      pure
        $ fmap Data.Text.unpack
        $ filter (not . Data.Text.null)
        $ Data.Text.lines text

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

  uniqSupply <- UniqSupply.mkSplitUniqSupply 'Q'
  let initialNameCache = NameCache.initNameCache uniqSupply []
  (_finalNameCache, hieFileResults) <- loadHieFiles initialNameCache hieFilePaths
  putStrLn $ ".hie files loaded: " <> (show . length) hieFileResults
  System.IO.hFlush System.IO.stdout

  pure ()
