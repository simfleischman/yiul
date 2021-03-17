module Yiul.Directory where

import qualified System.FilePath as FilePath

removeDotDirectories :: FilePath -> FilePath
removeDotDirectories = FilePath.joinPath . filter (/= ".") . FilePath.splitDirectories
