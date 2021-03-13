{-# LANGUAGE OverloadedStrings #-}

-- | Parsing ghc-pkg dump output
module Yiul.GhcPkg where

import Control.Applicative (Alternative (..))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Generics.Labels ()
import Data.Int (Int64)
import qualified Data.List as List
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.Word (Word8)
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Types.ComponentId as ComponentId
import Distribution.Types.InstalledPackageInfo (InstalledPackageInfo (..))
import qualified Distribution.Types.UnitId as UnitId
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified System.FilePath.Posix as FilePath.Posix

-- taken from http://hackage.haskell.org/package/Cabal-3.4.0.0/docs/src/Distribution.Simple.Program.HcPkg.html#parsePackages
-- but with left as error
parsePackages :: ByteString.Lazy.ByteString -> Either [String] [InstalledPackageInfo]
parsePackages lbs0 =
  case traverse InstalledPackageInfo.parseInstalledPackageInfo $ splitPkgs lbs0 of
    Right ok -> Right [setUnitId . maybe id mungePackagePaths (pkgRoot pkg) $ pkg | (_, pkg) <- ok]
    Left msgs -> Left (List.NonEmpty.toList msgs)
  where
    splitPkgs :: ByteString.Lazy.ByteString -> [ByteString.ByteString]
    splitPkgs = checkEmpty . doSplit
      where
        -- Handle the case of there being no packages at all.
        checkEmpty [s] | ByteString.all isSpace8 s = []
        checkEmpty ss = ss

        isSpace8 :: Word8 -> Bool
        isSpace8 9 = True -- '\t'
        isSpace8 10 = True -- '\n'
        isSpace8 13 = True -- '\r'
        isSpace8 32 = True -- ' '
        isSpace8 _ = False

        doSplit :: ByteString.Lazy.ByteString -> [ByteString.ByteString]
        doSplit lbs = go (ByteString.Lazy.findIndices (\w -> w == 10 || w == 13) lbs)
          where
            go :: [Int64] -> [ByteString.ByteString]
            go [] = [ByteString.Lazy.toStrict lbs]
            go (idx : idxs) =
              let (pfx, sfx) = ByteString.Lazy.splitAt idx lbs
               in case foldr ((<|>) . (`ByteString.Lazy.stripPrefix` sfx)) Nothing separators of
                    Just sfx' -> ByteString.Lazy.toStrict pfx : doSplit sfx'
                    Nothing -> go idxs

            separators :: [ByteString.Lazy.ByteString]
            separators = ["\n---\n", "\r\n---\r\n", "\r---\r"]

mungePackagePaths :: FilePath -> InstalledPackageInfo -> InstalledPackageInfo
-- Perform path/URL variable substitution as per the Cabal ${pkgroot} spec
-- (http://www.haskell.org/pipermail/libraries/2009-May/011772.html)
-- Paths/URLs can be relative to ${pkgroot} or ${pkgrooturl}.
-- The "pkgroot" is the directory containing the package database.
mungePackagePaths pkgroot pkginfo =
  pkginfo
    { importDirs = mungePaths (importDirs pkginfo),
      includeDirs = mungePaths (includeDirs pkginfo),
      libraryDirs = mungePaths (libraryDirs pkginfo),
      libraryDynDirs = mungePaths (libraryDynDirs pkginfo),
      frameworkDirs = mungePaths (frameworkDirs pkginfo),
      haddockInterfaces = mungePaths (haddockInterfaces pkginfo),
      haddockHTMLs = mungeUrls (haddockHTMLs pkginfo)
    }
  where
    mungePaths = map mungePath
    mungeUrls = map mungeUrl

    mungePath p = case stripVarPrefix "${pkgroot}" p of
      Just p' -> pkgroot </> p'
      Nothing -> p

    mungeUrl p = case stripVarPrefix "${pkgrooturl}" p of
      Just p' -> toUrlPath pkgroot p'
      Nothing -> p

    toUrlPath r p =
      "file:///"
        -- URLs always use posix style '/' separators:
        ++ FilePath.Posix.joinPath (r : FilePath.splitDirectories p)

    stripVarPrefix var p =
      case FilePath.splitPath p of
        (root : path') -> case List.stripPrefix var root of
          Just [sep] | FilePath.isPathSeparator sep -> Just (FilePath.joinPath path')
          _ -> Nothing
        _ -> Nothing

-- Older installed package info files did not have the installedUnitId
-- field, so if it is missing then we fill it as the source package ID.
-- NB: Internal libraries not supported.
setUnitId :: InstalledPackageInfo -> InstalledPackageInfo
setUnitId
  pkginfo@InstalledPackageInfo
    { installedUnitId = uid,
      sourcePackageId = pid
    }
    | UnitId.unUnitId uid == "" =
      pkginfo
        { installedUnitId = UnitId.mkLegacyUnitId pid,
          installedComponentId_ = ComponentId.mkComponentId (Pretty.prettyShow pid)
        }
setUnitId pkginfo = pkginfo
