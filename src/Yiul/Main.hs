{-# LANGUAGE BlockArguments #-}

module Yiul.Main where

import Control.Monad (when)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Generics.Labels ()
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import System.FilePath ((</>))
import qualified Yiul.GhcPkg
import qualified Yiul.Hie
import qualified Yiul.Report

main :: IO ()
main = do
  args <- Environment.getArgs
  inputPath <-
    case args of
      [arg] -> pure arg
      [] -> fail "Required argument: EITHER directory with any .hie files in subdirectories OR a file where each line is an absolute path to an .hie file"
      _ : _ : _ -> fail "Only pass one path argument"

  putStrLn $ "Loading ghc-pkg dump output: " <> inputPath
  bytes <- ByteString.Lazy.readFile inputPath
  case Yiul.GhcPkg.parsePackages bytes of
    Left errs -> do
      mapM_ putStrLn errs
      fail "See above errors parsing ghc-pkg dump output"
    Right results -> do
      putStrLn $ "Loaded " <> (show . length) results <> " package infos"

  when False do
    hieFilePaths <- Yiul.Hie.handleInputPath inputPath
    putStrLn $ ".hie files found: " <> (show . length) hieFilePaths

    putStrLn "Loading .hie files"
    hieFileResults <- Yiul.Hie.topLevelLoadHieFiles hieFilePaths

    let reportsDir = "reports"
    Directory.createDirectoryIfMissing True reportsDir
    Yiul.Report.writeReport (reportsDir </> "version-report.tsv") Yiul.Report.makeVersionReport hieFileResults
    Yiul.Report.checkHieVersions hieFileResults

    Yiul.Report.writeReport (reportsDir <> "stats-report.tsv") Yiul.Report.makeStatsReport hieFileResults

    Yiul.Report.processASTs hieFileResults
    Yiul.Report.writeReport (reportsDir <> "ast-report.tsv") Yiul.Report.makeAstStatsReport hieFileResults
