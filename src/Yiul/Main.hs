{-# LANGUAGE BlockArguments #-}

module Yiul.Main where

import Control.Monad (join, when)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Generics.Labels ()
import Options.Applicative
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
        <*> switch
          ( long "ast-report"
              <> short 'a'
              <> help "Generate a detailed AST report"
          )

run :: FilePath -> Maybe FilePath -> Maybe FilePath -> Bool -> IO ()
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

  when astReportFlag do
    Yiul.Report.processASTs hieFileResults
    Yiul.Report.writeReport (reportsDir </> "ast-report.tsv") Yiul.Report.makeAstStatsReport hieFileResults
