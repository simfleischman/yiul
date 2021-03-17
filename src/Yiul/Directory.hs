module Yiul.Directory where

import qualified System.FilePath as FilePath

removeDotDirectories :: FilePath -> FilePath
removeDotDirectories = FilePath.joinPath . filter (/= ".") . FilePath.splitDirectories

-- Changes
-- 'path/to/.stack-work/inner/stack/build/rest' to:
-- 'path/to/rest'
removeStackWorkThroughBuild :: FilePath -> FilePath
removeStackWorkThroughBuild input =
  let inputDirs = FilePath.splitDirectories input
      (beforeStackWork, stackWorkAndOn) = span (/= ".stack-work") inputDirs
      suffix = dropWhile (/= "build") stackWorkAndOn
      afterBuild =
        case suffix of
          buildName : rest | buildName == "build" -> rest
          _ -> suffix
   in FilePath.joinPath (beforeStackWork <> afterBuild)

-- Changes 'path/to/dir-name/dir-name-tmp' to 'path/to/dir-name', else leaves path as-is.
removeFinalNestedTmp :: FilePath -> FilePath
removeFinalNestedTmp input =
  let split = FilePath.splitDirectories input
   in case reverse split of
        possibleTmp : withoutTmp : rest | possibleTmp == withoutTmp <> "-tmp" -> FilePath.joinPath (reverse (withoutTmp : rest))
        _ -> input
