module Yiul.Directory where

import qualified System.FilePath as FilePath

removeDotDirectories :: FilePath -> FilePath
removeDotDirectories = FilePath.joinPath . filter (/= ".") . FilePath.splitDirectories

dropPrefixEndingWith :: FilePath -> FilePath -> FilePath
dropPrefixEndingWith lastDirOfPrefix input =
  let inputDirs = FilePath.splitDirectories input
      suffix = dropWhile (/= lastDirOfPrefix) inputDirs
      final =
        case suffix of
          [] -> suffix
          lastPrefix : result ->
            if lastPrefix == lastDirOfPrefix
              then result
              else suffix
   in FilePath.joinPath final

-- Changes 'dir-name/dir-name-tmp' to just 'dir-name', else leaves path as-is.
simplifyNestedTmp :: FilePath -> FilePath
simplifyNestedTmp input =
  let split = FilePath.splitDirectories input
   in case split of
        [left, right] | right == left <> "-tmp" -> left
        _ -> input
