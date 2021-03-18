{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Yiul.Main where

import Control.Monad (join, when)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Generics.Labels ()
import GHC.TypeLits (Symbol)
import Options.Applicative hiding (flag)
import qualified System.Directory as Directory
import Yiul.Const
import qualified Yiul.GhcPkg
import qualified Yiul.Graph
import qualified Yiul.Hie
import qualified Yiul.Report

main :: IO ()
main =
  join . customExecParser (prefs showHelpOnError) $
    info
      (helper <*> parser)
      ( fullDesc
          <> header "Yiul"
          <> progDesc "Haskell modularity tool"
      )
  where
    parser :: Parser (IO ())
    parser =
      run
        <$> strOption
          ( long "project-dir"
              <> short 'p'
              <> metavar "DIR"
              <> help "The directory of the Haskell project"
          )
        <*> optional
          ( strOption
              ( long "hie-files"
                  <> short 'h'
                  <> metavar "FILE"
                  <> help "A path to a file that lists the .hie files to process"
              )
          )
        <*> optional
          ( strOption
              ( long "ghc-pkg-dump"
                  <> short 'g'
                  <> metavar "FILE"
                  <> help "A path to a file that contains the dump of 'ghc-pkg dump'"
              )
          )
        <*> switchFlag @AstReportFlag
          ( long "ast-report"
              <> short 'a'
              <> help "Generate a detailed AST report"
          )
        <*> switchFlag @TopLevelBindingPackageReportFlag
          ( long "top-level-binding-package-report"
              <> short 'a'
              <> help "Generate a report of packages for top-level bindings"
          )
        <*> switchFlag @TopLevelBindingModuleReportFlag
          ( long "top-level-binding-module-report"
              <> short 'a'
              <> help "Generate a report of modules for top-level bindings"
          )

switchFlag :: forall t (s :: Symbol). (t ~ Const Bool s) => Mod FlagFields Bool -> Parser t
switchFlag = fmap (mkConst @t) . switch

run ::
  ProjectDir ->
  Maybe HieFileListPath ->
  Maybe GhcPkgDumpPath ->
  AstReportFlag ->
  TopLevelBindingPackageReportFlag ->
  TopLevelBindingModuleReportFlag ->
  IO ()
run
  projectDir
  mHieFileListPath
  mGhcPkgDump
  astReportFlag
  topLevelBindingPackageReportFlag
  topLevelBindingModuleReportFlag =
    do
      case mGhcPkgDump of
        Nothing -> pure ()
        Just ghcPkgDump ->
          do
            putStrLn $ "Loading ghc-pkg dump output: " <> unConst ghcPkgDump
            bytes <- ByteString.Lazy.readFile (unConst ghcPkgDump)
            case Yiul.GhcPkg.parsePackages bytes of
              Left errs -> do
                mapM_ putStrLn errs
                fail "See above errors parsing ghc-pkg dump output"
              Right results -> do
                putStrLn $ "Loaded " <> (show . length) results <> " package infos"

      hieFilePaths <-
        case mHieFileListPath of
          Nothing ->
            do
              putStrLn $ "Recursively finding files in " <> unConst projectDir
              Yiul.Hie.findHieFiles projectDir
          Just hieFileListPath -> Yiul.Hie.loadHieFileList projectDir hieFileListPath
      putStrLn $ ".hie files found: " <> (show . length) hieFilePaths

      hieFileResults <- Yiul.Hie.topLevelLoadHieFiles projectDir hieFilePaths

      let reportsDir :: ReportsPath
          reportsDir = "reports"
      Directory.createDirectoryIfMissing True (unConst reportsDir)
      Yiul.Report.writeReport (reportsDir </> "version-report.tsv") Yiul.Report.makeVersionReport hieFileResults
      Yiul.Report.checkHieVersions hieFileResults

      Yiul.Report.writeReport (reportsDir </> "stats-report.tsv") Yiul.Report.makeStatsReport hieFileResults

      whenFlag topLevelBindingPackageReportFlag do
        Yiul.Report.writeReport (reportsDir </> "top-level-binding-package-report.tsv") Yiul.Report.makeTopLevelBindingPackageReport hieFileResults

      whenFlag topLevelBindingModuleReportFlag do
        Yiul.Report.writeReport (reportsDir </> "top-level-binding-module-report.tsv") Yiul.Report.makeTopLevelBindingModuleReport hieFileResults

      whenFlag astReportFlag do
        Yiul.Report.processASTs hieFileResults
        Yiul.Report.writeReport (reportsDir </> "ast-report.tsv") Yiul.Report.makeAstStatsReport hieFileResults

      packageMap <- Yiul.Graph.organizeByPackages hieFileResults
      when False do
        Yiul.Report.writeReport (reportsDir </> "package-report.tsv") Yiul.Report.makePackagesReport packageMap
      let dependency = Yiul.Graph.buildModuleDependency packageMap
      forwardDependency <- Yiul.Graph.makeModuleDependencyClosure dependency
      reverseDependency <- Yiul.Graph.makeModuleReverseDependencyClosure dependency
      let twoWay = Yiul.Graph.joinForwardAndReverseDependencies forwardDependency reverseDependency
      Yiul.Report.writeReport (reportsDir </> "dependency-forward.tsv") Yiul.Report.makeForwardDependencyReport forwardDependency
      Yiul.Report.writeReport (reportsDir </> "dependency-reverse.tsv") Yiul.Report.makeReverseDependencyReport reverseDependency
      Yiul.Report.writeReport (reportsDir </> "dependency-summary.tsv") Yiul.Report.makeDependencyReportSummary twoWay

      pure ()
