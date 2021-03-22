{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Yiul.Const where

import Control.Applicative
import Control.Monad (when)
import Data.String (IsString)
import Data.Text (Text)
import GHC.TypeLits (Symbol)
import qualified System.FilePath as FilePath

type ProjectDir = Const FilePath "ProjectDir"

type ReportsPath = Const FilePath "ReportsPath"

type HieFileListPath = Const FilePath "HieFileListPath"

type GhcPkgDumpPath = Const FilePath "GhcPkgDumpPath"

type HieFilePath = Const FilePath "HieFilePath"

type AstReportFlag = Const Bool "AstReportFlag"

type TopLevelBindingPackageReportFlag = Const Bool "TopLevelBindingPackageReportFlag"

type TopLevelBindingModuleReportFlag = Const Bool "TopLevelBindingModuleReportFlag"

type ModuleName = Const Text "ModuleName"

type LibraryId = Const Text "LibraryId"

type PackageExeId = Const FilePath "PackageExeId"

type JsonOutputFlag = Const Bool "JsonOutputFlag"

type PrettyJsonFlag = Const Bool "PrettyJsonFlag"

mkConst :: forall t (s :: Symbol) a. (t ~ Const a s) => a -> t
mkConst = Const

unConst :: forall a (s :: Symbol). Const a s -> a
unConst = getConst

whenFlag :: forall (s :: Symbol) m. Applicative m => Const Bool s -> m () -> m ()
whenFlag = when . unConst

makeRelativeFilePath,
  (</>) ::
    forall tDir tFile (sDir :: Symbol) (sFile :: Symbol).
    (tDir ~ Const FilePath sDir, tFile ~ Const FilePath sFile) =>
    tDir ->
    tFile ->
    tFile
makeRelativeFilePath p q = mkConst (FilePath.makeRelative (unConst p) (unConst q))
(</>) p q = mkConst (unConst p FilePath.</> unConst q)

deriving newtype instance (IsString k) => IsString (Const k (s :: Symbol))
