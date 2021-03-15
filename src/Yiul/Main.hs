{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
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
import System.FilePath ((</>))
import qualified Yiul.GhcPkg
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

type AstReportFlag = Const Bool "AstReportFlag"

mkConst :: forall t (s :: Symbol) a. (t ~ Const a s) => a -> t
mkConst = Const

unConst :: forall a (s :: Symbol). Const a s -> a
unConst = getConst

whenFlag :: forall (s :: Symbol) m. Applicative m => Const Bool s -> m () -> m ()
whenFlag flag = when (unConst flag)

switchFlag :: forall t (s :: Symbol). (t ~ Const Bool s) => Mod FlagFields Bool -> Parser t
switchFlag = fmap (mkConst @t) . switch

run :: FilePath -> Maybe FilePath -> Maybe FilePath -> AstReportFlag -> IO ()
run projectDir mHieFileListPath mGhcPkgDump astReportFlag = do
  case mGhcPkgDump of
    Nothing -> pure ()
    Just ghcPkgDump -> do
      putStrLn $ "Loading ghc-pkg dump output: " <> ghcPkgDump
      bytes <- ByteString.Lazy.readFile ghcPkgDump
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
          putStrLn $ "Recursively finding files in " <> projectDir
          Yiul.Hie.findHieFiles projectDir
      Just hieFileListPath -> Yiul.Hie.loadHieFileList projectDir hieFileListPath
  putStrLn $ ".hie files found: " <> (show . length) hieFilePaths

  hieFileResults <- Yiul.Hie.topLevelLoadHieFiles projectDir hieFilePaths

  let reportsDir = "reports"
  Directory.createDirectoryIfMissing True reportsDir
  Yiul.Report.writeReport (reportsDir </> "version-report.tsv") Yiul.Report.makeVersionReport hieFileResults
  Yiul.Report.checkHieVersions hieFileResults

  Yiul.Report.writeReport (reportsDir </> "stats-report.tsv") Yiul.Report.makeStatsReport hieFileResults

  Yiul.Report.writeReport (reportsDir </> "top-level-binding-report.tsv") Yiul.Report.makeTopLevelBindingReport hieFileResults

  whenFlag astReportFlag do
    Yiul.Report.processASTs hieFileResults
    Yiul.Report.writeReport (reportsDir </> "ast-report.tsv") Yiul.Report.makeAstStatsReport hieFileResults
