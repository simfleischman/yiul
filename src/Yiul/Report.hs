{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Yiul.Report where

import qualified Avail
import qualified Control.Lens as Lens
import Control.Monad (when)
import qualified Data.Array as Array
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Generics.Labels ()
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified FastString
import HieBin (HieFileResult)
import qualified HieBin
import qualified HieTypes
import qualified Module
import qualified Name
import qualified SrcLoc
import qualified UniqSet
import Yiul.Const
import Prelude hiding (span)

makeVersionReport :: [(HieFilePath, HieFileResult)] -> Text
makeVersionReport = makeTsv . (headerLine :) . fmap makeLine
  where
    headerLine =
      ( "HIE File",
        "Haskell Source File",
        "HIE File Version",
        "GHC Version"
      )
    makeLine (filePath, hieFileResult) =
      let hieFile = HieBin.hie_file_result hieFileResult
       in ( (Text.pack . unConst) filePath,
            (Text.pack . HieTypes.hie_hs_file) hieFile,
            (Text.pack . show . HieBin.hie_file_result_version) hieFileResult,
            (Text.Encoding.decodeUtf8 . HieBin.hie_file_result_ghc_version) hieFileResult
          )

makeStatsReport :: [(HieFilePath, HieFileResult)] -> Text
makeStatsReport = makeTsv . (headerLine :) . fmap makeLine
  where
    headerLine =
      ( "HIE File",
        "Haskell Source File",
        "AST filepath",
        "Module UnitId",
        "Module Name",
        "Types used",
        "Exports",
        "AST filepath count",
        "AST top-level children"
      )
    makeLine (filePath, hieFileResult) =
      let hieFile = HieBin.hie_file_result hieFileResult
          hieModule = HieTypes.hie_module hieFile
       in ( (Text.pack . unConst) filePath,
            (Text.pack . HieTypes.hie_hs_file) hieFile,
            getFirstAstFile hieFile,
            (makeUnitIdText . Module.moduleUnitId) hieModule,
            (Text.pack . Module.moduleNameString . Module.moduleName) hieModule,
            (Text.pack . show . (\(high, low) -> abs (high - low) + 1) . Array.bounds . HieTypes.hie_types) hieFile,
            (Text.pack . show . UniqSet.sizeUniqSet . Avail.availsToNameSet . HieTypes.hie_exports) hieFile,
            (Text.pack . show . Map.size . HieTypes.getAsts . HieTypes.hie_asts) hieFile,
            (Text.pack . show . initialAstChildrenCount) hieFile
          )

-- | The Map always seems to have 1 or 0 elements.
getFirstAstFile :: HieTypes.HieFile -> Text
getFirstAstFile hieFile =
  let astMap = (HieTypes.getAsts . HieTypes.hie_asts) hieFile
   in case Map.assocs astMap of
        [] -> "No ASTs"
        (astPath, _) : _ -> (Text.pack . FastString.unpackFS) astPath

initialAstChildrenCount :: HieTypes.HieFile -> Int
initialAstChildrenCount hieFile = maybe 0 (length . HieTypes.nodeChildren) (getMaybeAst hieFile)

getMaybeAst :: HieTypes.HieFile -> Maybe (HieTypes.HieAST HieTypes.TypeIndex)
getMaybeAst hieFile =
  let astMap = (HieTypes.getAsts . HieTypes.hie_asts) hieFile
   in case Map.assocs astMap of
        [] -> Nothing
        (_, ast) : _ -> Just ast

makeUnitIdText :: Module.UnitId -> Text
makeUnitIdText (Module.IndefiniteUnitId _) = "IndefiniteUnitId"
makeUnitIdText (Module.DefiniteUnitId defUnitId) = (Text.pack . FastString.unpackFS . Module.installedUnitIdFS . Module.unDefUnitId) defUnitId

writeReport :: ReportsPath -> ([(HieFilePath, HieFileResult)] -> Text) -> [(HieFilePath, HieFileResult)] -> IO ()
writeReport reportPath makeReport hieFileResults = do
  putStrLn $ "Writing " <> unConst reportPath
  ByteString.writeFile (unConst reportPath) $ Text.Encoding.encodeUtf8 $ makeReport hieFileResults

-- | Index by HIE version (Integer) and GHC version (ByteString)
makeVersionMap :: [(HieFilePath, HieFileResult)] -> Map (Integer, ByteString) [(HieFilePath, HieFileResult)]
makeVersionMap = Map.unionsWith (<>) . fmap go
  where
    go pair@(_, hieFileResult) =
      Map.singleton
        (HieBin.hie_file_result_version hieFileResult, HieBin.hie_file_result_ghc_version hieFileResult)
        [pair]

checkHieVersions :: [(HieFilePath, HieFileResult)] -> IO ()
checkHieVersions hieFileResults = do
  let versionMap = makeVersionMap hieFileResults
      versionMapKeys = Map.keys versionMap
  case versionMapKeys of
    [] -> fail "No .hie files found"
    [(hieVersion, _ghcVersion)] -> do
      when (HieTypes.hieVersion /= hieVersion) do
        fail $ "Our HIE version: " <> show HieTypes.hieVersion <> " does not match .hie file version: " <> show hieVersion
    _ : _ : _ -> do
      putStrLn "Multiple versions of HIE/GHC found. See version report for details."
      mapM_ (\(hieVersion, ghcVersion) -> putStrLn $ show hieVersion <> " / " <> (Text.unpack . Text.Encoding.decodeUtf8) ghcVersion) versionMapKeys

processASTs :: [(HieFilePath, HieFileResult)] -> IO ()
processASTs hieFileResults = do
  let astFilePathSet = foldr buildAstFilePathSet Set.empty hieFileResults
  putStrLn $ "AST key count: " <> (show . Set.size) astFilePathSet

  when False do
    mapM_ (putStrLn . FastString.unpackFS) astFilePathSet

  let topLevelAsts = concatMap (maybe [] pure . getMaybeAst . HieBin.hie_file_result . snd) hieFileResults
      topLevelNodeInfos = HieTypes.nodeInfo <$> topLevelAsts
  let topLevelNodePairs = foldr buildNodeConstructorNodeTypePairCount Map.empty topLevelNodeInfos
  putStrLn $ "Top-level node constructor/type pair count: " <> (show . Map.size) topLevelNodePairs
  mapM_
    (\(pair, ct) -> putStrLn $ pairToString pair <> " : " <> show ct)
    (Map.assocs topLevelNodePairs)

  let subModuleTopLevelNodeInfos = HieTypes.nodeInfo <$> concatMap HieTypes.nodeChildren topLevelAsts
      subModuleTopLevelNodeInfoMap = foldr buildAnnotationPairs Map.empty subModuleTopLevelNodeInfos
  putStrLn $ "Sub-module node constructor/type pair count: " <> (show . Map.size) subModuleTopLevelNodeInfoMap
  mapM_
    (\(pairSet, ct) -> putStrLn $ (List.intercalate ", " . fmap pairToString . Set.toList) pairSet <> " : " <> show ct)
    (Map.assocs subModuleTopLevelNodeInfoMap)
  where
    pairToString (ctr, typ) = FastString.unpackFS ctr <> "/" <> FastString.unpackFS typ
    buildAstFilePathSet (_hiePath, hieFileResult) inputSet =
      let astMap = (HieTypes.getAsts . HieTypes.hie_asts . HieBin.hie_file_result) hieFileResult
       in Set.union inputSet (Map.keysSet astMap)

    buildNodeConstructorNodeTypePairCount nodeInfo inputMap =
      let maps = Map.fromSet (const (1 :: Int)) (HieTypes.nodeAnnotations nodeInfo)
       in Map.unionsWith (+) [inputMap, maps]

    buildAnnotationPairs nodeInfo inputMap =
      let maps = Map.singleton (HieTypes.nodeAnnotations nodeInfo) (1 :: Int)
       in Map.unionsWith (+) [inputMap, maps]

makeTsv :: Lens.Each s s Text Text => [s] -> Text
makeTsv = Text.unlines . fmap (Text.intercalate "\t" . Lens.toListOf Lens.each)

makeAstStatsReport :: [(HieFilePath, HieFileResult)] -> Text
makeAstStatsReport = makeTsv . (headerLine :) . concatMap handlePair
  where
    headerLine =
      ( "Span",
        "End",
        "Node Annotations",
        "Node Children Count",
        "Node Type Count",
        "Modules",
        "Node Identifiers"
      )
    handlePair (filePath, hieFileResult) =
      case (getMaybeAst . HieBin.hie_file_result) hieFileResult of
        Nothing -> []
        Just ast -> makeAstLines filePath ast
    makeAstLines filePath ast =
      let nodeInfo = HieTypes.nodeInfo ast
          nodeAnnotationsText =
            Text.intercalate ", "
              . fmap (\(c, t) -> (Text.pack . FastString.unpackFS) c <> "/" <> (Text.pack . FastString.unpackFS) t)
              . Set.toList
              . HieTypes.nodeAnnotations
              $ nodeInfo
          nodeIdentifiersText =
            Text.intercalate "; "
              . fmap (\(identifier, details) -> identifierToText identifier <> " " <> (Text.pack . show . Set.toList . HieTypes.identInfo) details)
              . Map.assocs
              $ HieTypes.nodeIdentifiers nodeInfo
          modulesText =
            Text.intercalate "; "
              . fmap identifierToModuleText
              . Map.keys
              $ HieTypes.nodeIdentifiers nodeInfo
          currentLine =
            ( (realSrcLocToText . SrcLoc.realSrcSpanStart . HieTypes.nodeSpan) ast,
              (realSrcLocToLineColText . SrcLoc.realSrcSpanEnd . HieTypes.nodeSpan) ast,
              nodeAnnotationsText,
              (Text.pack . show . length . HieTypes.nodeChildren) ast,
              (Text.pack . show . length . HieTypes.nodeType . HieTypes.nodeInfo) ast,
              modulesText,
              nodeIdentifiersText
            )
          nextLines = concatMap (makeAstLines filePath) (HieTypes.nodeChildren ast)
       in currentLine : nextLines

makeTopLevelBindingReport :: [(HieFilePath, HieFileResult)] -> Text
makeTopLevelBindingReport = makeTsv . (headerLine :) . concatMap handlePair
  where
    headerLine =
      ( "Span",
        "End",
        "Node Annotations",
        "Descendants",
        "Lines",
        "UnitId Count",
        "UnitIds"
      )
    handlePair (filePath, hieFileResult) =
      case (getMaybeAst . HieBin.hie_file_result) hieFileResult of
        Nothing -> []
        Just ast ->
          if (HieTypes.nodeAnnotations . HieTypes.nodeInfo) ast == Set.singleton ("Module", "Module")
            then
              concatMap
                (makeLines filePath)
                $ filter
                  ( Set.null
                      . Set.intersection
                        (Set.fromList ["IEName", "IEThingWith", "IEThingAll", "IEModuleContents", "ImportDecl"]) -- ignore exports and imports
                      . Set.map fst
                      . HieTypes.nodeAnnotations
                      . HieTypes.nodeInfo
                  )
                  $ HieTypes.nodeChildren ast
            else makeLines filePath ast
    recursiveAstCount ast = (1 :: Int) + sum (recursiveAstCount <$> HieTypes.nodeChildren ast)
    identifierToUnitIdSet (Left _moduleName) = Set.empty
    identifierToUnitIdSet (Right name) = Set.fromList $ fmap Module.moduleUnitId $ Maybe.maybeToList $ Name.nameModule_maybe name
    unitIdsInAst ast =
      Set.unions
        ( fmap
            identifierToUnitIdSet
            (Map.keys . HieTypes.nodeIdentifiers . HieTypes.nodeInfo $ ast)
        )
        <> Set.unions (fmap unitIdsInAst (HieTypes.nodeChildren ast))
    makeLines _filePath ast =
      let nodeInfo = HieTypes.nodeInfo ast
          nodeAnnotationsText =
            Text.intercalate ", "
              . fmap (\(c, t) -> (Text.pack . FastString.unpackFS) c <> "/" <> (Text.pack . FastString.unpackFS) t)
              . Set.toList
              . HieTypes.nodeAnnotations
              $ nodeInfo
          unitIds = unitIdsInAst ast
          currentLine =
            ( (realSrcLocToText . SrcLoc.realSrcSpanStart . HieTypes.nodeSpan) ast,
              (realSrcLocToLineColText . SrcLoc.realSrcSpanEnd . HieTypes.nodeSpan) ast,
              nodeAnnotationsText,
              (Text.pack . show . recursiveAstCount) ast,
              (Text.pack . show . (\span -> 1 + SrcLoc.srcSpanEndLine span - SrcLoc.srcSpanStartLine span) . HieTypes.nodeSpan) ast,
              (Text.pack . show . Set.size) unitIds,
              (Text.intercalate ", " . fmap makeUnitIdText . Set.toList) unitIds
            )
       in [currentLine]

identifierToText :: Either Module.ModuleName Name.Name -> Text
identifierToText (Left moduleName) = (Text.pack . Module.moduleNameString) moduleName
identifierToText (Right name) = (Text.pack . Name.nameStableString) name

identifierToModuleText :: Either Module.ModuleName Name.Name -> Text
identifierToModuleText (Left moduleName) =
  "ModuleName: " <> (Text.pack . Module.moduleNameString) moduleName
identifierToModuleText (Right name) =
  case Name.nameModule_maybe name of
    Nothing -> "NameNoModule: " <> (Text.pack . Name.nameStableString) name
    Just (Module.Module unitId moduleName) -> "UnitId: " <> makeUnitIdText unitId <> ", module: " <> (Text.pack . Module.moduleNameString) moduleName

realSrcSpanToText :: SrcLoc.RealSrcSpan -> Text
realSrcSpanToText srcSpan =
  (realSrcLocToText . SrcLoc.realSrcSpanStart) srcSpan
    <> "-"
    <> (Text.pack . show . SrcLoc.srcLocLine . SrcLoc.realSrcSpanEnd) srcSpan
    <> ":"
    <> (Text.pack . show . SrcLoc.srcLocCol . SrcLoc.realSrcSpanEnd) srcSpan

realSrcLocToText :: SrcLoc.RealSrcLoc -> Text
realSrcLocToText loc =
  (Text.pack . FastString.unpackFS . SrcLoc.srcLocFile) loc
    <> ":"
    <> (Text.pack . show . SrcLoc.srcLocLine) loc
    <> ":"
    <> (Text.pack . show . SrcLoc.srcLocCol) loc

realSrcLocToLineColText :: SrcLoc.RealSrcLoc -> Text
realSrcLocToLineColText loc =
  (Text.pack . show . SrcLoc.srcLocLine) loc
    <> ":"
    <> (Text.pack . show . SrcLoc.srcLocCol) loc

srcLocToText :: SrcLoc.SrcLoc -> Text
srcLocToText (SrcLoc.RealSrcLoc loc) =
  (Text.pack . FastString.unpackFS . SrcLoc.srcLocFile) loc
    <> ":"
    <> (Text.pack . show . SrcLoc.srcLocLine) loc
    <> ":"
    <> (Text.pack . show . SrcLoc.srcLocCol) loc
srcLocToText (SrcLoc.UnhelpfulLoc fastString) = "UnhelpfulLoc:" <> (Text.pack . FastString.unpackFS) fastString
