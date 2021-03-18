{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Yiul.Report where

import qualified Avail
import qualified Control.Lens as Lens
import Control.Monad (when)
import Data.Array ((!))
import qualified Data.Array as Array
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import Data.Generics.Labels ()
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified FastString
import HieBin (HieFileResult)
import qualified HieBin
import qualified HieTypes
import qualified HieUtils
import qualified IfaceType
import qualified Module
import qualified Name
import qualified SrcLoc
import qualified System.FilePath as FilePath
import qualified UniqSet
import Yiul.Const
import qualified Yiul.Directory
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
            (Text.pack . Yiul.Directory.removeDotDirectories . HieTypes.hie_hs_file) hieFile,
            (Text.pack . show . HieBin.hie_file_result_version) hieFileResult,
            (Text.Encoding.decodeUtf8 . HieBin.hie_file_result_ghc_version) hieFileResult
          )

makeStatsReport :: [(HieFilePath, HieFileResult)] -> Text
makeStatsReport = makeTsv . (headerLine :) . fmap makeLine
  where
    headerLine =
      ( "HIE File",
        "Haskell Source File",
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
            (Text.pack . Yiul.Directory.removeDotDirectories . HieTypes.hie_hs_file) hieFile,
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

writeReport :: ReportsPath -> (a -> Text) -> a -> IO ()
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
        "Module",
        "Node Annotations",
        "Node Children Count",
        "Node Type Count",
        "Modules",
        "Node Identifiers"
      )
    handlePair (filePath, hieFileResult) =
      let hieFile = HieBin.hie_file_result hieFileResult
       in case getMaybeAst hieFile of
            Nothing -> []
            Just ast -> makeAstLines filePath hieFile ast
    makeAstLines filePath hieFile ast =
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
          moduleValue = HieTypes.hie_module hieFile

          currentLine =
            ( (realSrcLocToText . SrcLoc.realSrcSpanStart . HieTypes.nodeSpan) ast,
              (realSrcLocToLineColText . SrcLoc.realSrcSpanEnd . HieTypes.nodeSpan) ast,
              (Text.pack . Module.moduleStableString) moduleValue,
              nodeAnnotationsText,
              (Text.pack . show . length . HieTypes.nodeChildren) ast,
              (Text.pack . show . length . HieTypes.nodeType . HieTypes.nodeInfo) ast,
              modulesText,
              nodeIdentifiersText
            )
          nextLines = concatMap (makeAstLines filePath hieFile) (HieTypes.nodeChildren ast)
       in currentLine : nextLines

makeTopLevelBindingPackageReport :: [(HieFilePath, HieFileResult)] -> Text
makeTopLevelBindingPackageReport = makeTsv . (headerLine :) . concatMap handlePair
  where
    headerLine =
      ( "Span",
        "End",
        "Node Annotations",
        "Descendants",
        "Lines",
        "Term UnitId Count",
        "Term UnitIds",
        "Type All UnitId Count",
        "Type All UnitIds",
        "Type Visible UnitId Count",
        "Type Visible UnitIds",
        "Combined All UnitId Count",
        "Combined Visible UnitId Count"
      )
    handlePair (_filePath, hieFileResult) =
      let hieFile = HieBin.hie_file_result hieFileResult
       in case getMaybeAst hieFile of
            Nothing -> []
            Just ast ->
              if (HieTypes.nodeAnnotations . HieTypes.nodeInfo) ast == Set.singleton ("Module", "Module")
                then
                  concatMap
                    (makeLines hieFile)
                    $ filter
                      ( Set.null
                          . Set.intersection
                            (Set.fromList ["IEName", "IEThingWith", "IEThingAll", "IEModuleContents", "ImportDecl"]) -- ignore exports and imports
                          . Set.map fst
                          . HieTypes.nodeAnnotations
                          . HieTypes.nodeInfo
                      )
                      $ HieTypes.nodeChildren ast
                else makeLines hieFile ast
    recursiveAstCount ast = (1 :: Int) + sum (recursiveAstCount <$> HieTypes.nodeChildren ast)
    makeLines hieFile ast =
      let nodeInfo = HieTypes.nodeInfo ast
          nodeAnnotationsText =
            Text.intercalate ", "
              . fmap (\(c, t) -> (Text.pack . FastString.unpackFS) c <> "/" <> (Text.pack . FastString.unpackFS) t)
              . Set.toList
              . HieTypes.nodeAnnotations
              $ nodeInfo
          termUnitIds = termUnitIdSet ast
          allTypeUnitIds = collectFromTypeIndexSet collectUnitIdsForHieType VisibleAndInvsibleArgs hieFile (getTypeIndexesRecursively combinedLocalTypeIndexes ast)
          visibleTypeUnitIds = collectFromTypeIndexSet collectUnitIdsForHieType OnlyVisibleArgs hieFile (getTypeIndexesRecursively combinedLocalTypeIndexes ast)
          currentLine =
            ( (realSrcLocToText . SrcLoc.realSrcSpanStart . HieTypes.nodeSpan) ast,
              (realSrcLocToLineColText . SrcLoc.realSrcSpanEnd . HieTypes.nodeSpan) ast,
              nodeAnnotationsText,
              (Text.pack . show . recursiveAstCount) ast,
              (Text.pack . show . (\span -> 1 + SrcLoc.srcSpanEndLine span - SrcLoc.srcSpanStartLine span) . HieTypes.nodeSpan) ast,
              (Text.pack . show . Set.size) termUnitIds,
              (Text.intercalate ", " . fmap makeUnitIdText . Set.toList) termUnitIds,
              (Text.pack . show . Set.size) allTypeUnitIds,
              (Text.intercalate ", " . fmap makeUnitIdText . Set.toList) allTypeUnitIds,
              (Text.pack . show . Set.size) visibleTypeUnitIds,
              (Text.intercalate ", " . fmap makeUnitIdText . Set.toList) visibleTypeUnitIds,
              (Text.pack . show . Set.size) (Set.union termUnitIds allTypeUnitIds),
              (Text.pack . show . Set.size) (Set.union termUnitIds visibleTypeUnitIds)
            )
       in [currentLine]

makeTopLevelBindingModuleReport :: [(HieFilePath, HieFileResult)] -> Text
makeTopLevelBindingModuleReport = makeTsv . (headerLine :) . concatMap handlePair
  where
    headerLine =
      ( "Span",
        "End",
        "Module",
        "Node Annotations",
        "Descendants",
        "Lines",
        "Node Type Module Count",
        "Identifier Type Module Count",
        "Term Module Count"
      )
    handlePair (_filePath, hieFileResult) =
      let hieFile = HieBin.hie_file_result hieFileResult
       in case getMaybeAst hieFile of
            Nothing -> []
            Just ast ->
              if (HieTypes.nodeAnnotations . HieTypes.nodeInfo) ast == Set.singleton ("Module", "Module")
                then
                  concatMap
                    (makeLines hieFile)
                    $ filter
                      ( Set.null
                          . Set.intersection
                            (Set.fromList ["IEName", "IEThingWith", "IEThingAll", "IEModuleContents", "ImportDecl"]) -- ignore exports and imports
                          . Set.map fst
                          . HieTypes.nodeAnnotations
                          . HieTypes.nodeInfo
                      )
                      $ HieTypes.nodeChildren ast
                else makeLines hieFile ast
    recursiveAstCount ast = (1 :: Int) + sum (recursiveAstCount <$> HieTypes.nodeChildren ast)
    makeLines hieFile ast =
      let nodeInfo = HieTypes.nodeInfo ast
          nodeAnnotationsText =
            Text.intercalate ", "
              . fmap (\(c, t) -> (Text.pack . FastString.unpackFS) c <> "/" <> (Text.pack . FastString.unpackFS) t)
              . Set.toList
              . HieTypes.nodeAnnotations
              $ nodeInfo
          termModules = termModuleSet ast
          nodeTypeModules = collectFromTypeIndexSet collectModulesForHieType OnlyVisibleArgs hieFile (getTypeIndexesRecursively localNodeTypeIndexes ast)
          identifierTypeModules = collectFromTypeIndexSet collectModulesForHieType OnlyVisibleArgs hieFile (getTypeIndexesRecursively localIdentifierTypeIndexes ast)
          currentLine =
            ( (realSrcLocToText . SrcLoc.realSrcSpanStart . HieTypes.nodeSpan) ast,
              (realSrcLocToLineColText . SrcLoc.realSrcSpanEnd . HieTypes.nodeSpan) ast,
              (Text.pack . Module.moduleStableString . HieTypes.hie_module) hieFile,
              nodeAnnotationsText,
              (Text.pack . show . recursiveAstCount) ast,
              (Text.pack . show . (\span -> 1 + SrcLoc.srcSpanEndLine span - SrcLoc.srcSpanStartLine span) . HieTypes.nodeSpan) ast,
              (Text.pack . show . Set.size) nodeTypeModules,
              (Text.pack . show . Set.size) identifierTypeModules,
              (Text.pack . show . Set.size) termModules
            )
       in [currentLine]

nameToUnitIdSet :: Name.Name -> Set Module.UnitId
nameToUnitIdSet = Set.fromList . fmap Module.moduleUnitId . Maybe.maybeToList . Name.nameModule_maybe

combinedLocalTypeIndexes :: HieTypes.HieAST HieTypes.TypeIndex -> Set HieTypes.TypeIndex
combinedLocalTypeIndexes ast = localNodeTypeIndexes ast <> localIdentifierTypeIndexes ast

localNodeTypeIndexes :: HieTypes.HieAST HieTypes.TypeIndex -> Set HieTypes.TypeIndex
localNodeTypeIndexes = Set.fromList . HieTypes.nodeType . HieTypes.nodeInfo

localIdentifierTypeIndexes :: HieTypes.HieAST HieTypes.TypeIndex -> Set HieTypes.TypeIndex
localIdentifierTypeIndexes = Set.fromList . Maybe.mapMaybe HieTypes.identType . Map.elems . HieTypes.nodeIdentifiers . HieTypes.nodeInfo

getTypeIndexesRecursively ::
  ( HieTypes.HieAST HieTypes.TypeIndex ->
    Set HieTypes.TypeIndex
  ) ->
  HieTypes.HieAST HieTypes.TypeIndex ->
  Set HieTypes.TypeIndex
getTypeIndexesRecursively localGetIndexes ast =
  let nodeChildren = HieTypes.nodeChildren ast
      childrenSets = getTypeIndexesRecursively localGetIndexes <$> nodeChildren
   in localGetIndexes ast <> Set.unions childrenSets

data VisibleArgs = VisibleAndInvsibleArgs | OnlyVisibleArgs | OnlyInvisibleArgs

hieArgsToIndexSet :: VisibleArgs -> HieTypes.HieArgs HieTypes.TypeIndex -> Set HieTypes.TypeIndex
hieArgsToIndexSet VisibleAndInvsibleArgs (HieTypes.HieArgs pairs) = Set.fromList (snd <$> pairs)
hieArgsToIndexSet OnlyVisibleArgs (HieTypes.HieArgs pairs) = Set.fromList (snd <$> filter fst pairs)
hieArgsToIndexSet OnlyInvisibleArgs (HieTypes.HieArgs pairs) = Set.fromList (snd <$> filter (not . fst) pairs)

collectUnitIdsForHieType :: HieTypes.HieType HieTypes.TypeIndex -> Set Module.UnitId
collectUnitIdsForHieType (HieTypes.HTyVarTy name) = nameToUnitIdSet name
collectUnitIdsForHieType (HieTypes.HAppTy _index1 _args) = Set.empty
collectUnitIdsForHieType (HieTypes.HTyConApp ifaceTyCon _args) = (nameToUnitIdSet . IfaceType.ifaceTyConName) ifaceTyCon
collectUnitIdsForHieType (HieTypes.HForAllTy ((name, _index1), _argFlag) _index2) = nameToUnitIdSet name
collectUnitIdsForHieType (HieTypes.HFunTy _index1 _index2) = Set.empty
collectUnitIdsForHieType (HieTypes.HQualTy _index1 _index2) = Set.empty
collectUnitIdsForHieType (HieTypes.HLitTy _ifaceTyLit) = Set.empty
collectUnitIdsForHieType (HieTypes.HCastTy _index) = Set.empty
collectUnitIdsForHieType HieTypes.HCoercionTy = Set.empty

collectNamesForHieType :: HieTypes.HieType HieTypes.TypeIndex -> Set Name.Name
collectNamesForHieType (HieTypes.HTyVarTy name) = Set.singleton name
collectNamesForHieType (HieTypes.HAppTy _index1 _args) = Set.empty
collectNamesForHieType (HieTypes.HTyConApp ifaceTyCon _args) = (Set.singleton . IfaceType.ifaceTyConName) ifaceTyCon
collectNamesForHieType (HieTypes.HForAllTy ((name, _index1), _argFlag) _index2) = Set.singleton name
collectNamesForHieType (HieTypes.HFunTy _index1 _index2) = Set.empty
collectNamesForHieType (HieTypes.HQualTy _index1 _index2) = Set.empty
collectNamesForHieType (HieTypes.HLitTy _ifaceTyLit) = Set.empty
collectNamesForHieType (HieTypes.HCastTy _index) = Set.empty
collectNamesForHieType HieTypes.HCoercionTy = Set.empty

maybeToSet :: Maybe a -> Set a
maybeToSet Nothing = Set.empty
maybeToSet (Just x) = Set.singleton x

collectModulesForHieType :: HieTypes.HieType HieTypes.TypeIndex -> Set Module.Module
collectModulesForHieType (HieTypes.HTyVarTy name) = (maybeToSet . Name.nameModule_maybe) name
collectModulesForHieType (HieTypes.HAppTy _index1 _args) = Set.empty
collectModulesForHieType (HieTypes.HTyConApp ifaceTyCon _args) = (maybeToSet . Name.nameModule_maybe . IfaceType.ifaceTyConName) ifaceTyCon
collectModulesForHieType (HieTypes.HForAllTy ((name, _index1), _argFlag) _index2) = (maybeToSet . Name.nameModule_maybe) name
collectModulesForHieType (HieTypes.HFunTy _index1 _index2) = Set.empty
collectModulesForHieType (HieTypes.HQualTy _index1 _index2) = Set.empty
collectModulesForHieType (HieTypes.HLitTy _ifaceTyLit) = Set.empty
collectModulesForHieType (HieTypes.HCastTy _index) = Set.empty
collectModulesForHieType HieTypes.HCoercionTy = Set.empty

extraIndexesForHieType :: VisibleArgs -> HieTypes.HieType HieTypes.TypeIndex -> Set HieTypes.TypeIndex
extraIndexesForHieType _visibleArgs (HieTypes.HTyVarTy _name) = Set.empty
extraIndexesForHieType visibleArgs (HieTypes.HAppTy index1 args) = Set.insert index1 (hieArgsToIndexSet visibleArgs args)
extraIndexesForHieType visibleArgs (HieTypes.HTyConApp _ifaceTyCon args) = hieArgsToIndexSet visibleArgs args
extraIndexesForHieType _visibleArgs (HieTypes.HForAllTy ((_name, index1), _argFlag) index2) = Set.fromList [index1, index2]
extraIndexesForHieType _visibleArgs (HieTypes.HFunTy index1 index2) = Set.fromList [index1, index2]
extraIndexesForHieType _visibleArgs (HieTypes.HQualTy index1 index2) = Set.fromList [index1, index2]
extraIndexesForHieType _visibleArgs (HieTypes.HLitTy _ifaceTyLit) = Set.empty
extraIndexesForHieType _visibleArgs (HieTypes.HCastTy index) = Set.singleton index
extraIndexesForHieType _visibleArgs HieTypes.HCoercionTy = Set.empty

collectFromTypeIndexSet :: Monoid m => (HieTypes.HieType HieTypes.TypeIndex -> m) -> VisibleArgs -> HieTypes.HieFile -> Set HieTypes.TypeIndex -> m
collectFromTypeIndexSet collect visibleArgs hieFile typeIndexSet = go Set.empty typeIndexSet mempty
  where
    go visited toVisit resultSet =
      if Set.null toVisit
        then resultSet
        else
          let (currentSet, moreToVisit) = Set.splitAt 1 toVisit
              currentIndex = Set.elemAt 0 currentSet
           in if Set.member currentIndex visited
                then go visited moreToVisit resultSet
                else
                  let typ = HieTypes.hie_types hieFile ! currentIndex
                      currentResults = collect typ
                      extraToVisit = extraIndexesForHieType visibleArgs typ
                      extraToVisitMinusCurrent = Set.delete currentIndex extraToVisit
                   in go (Set.insert currentIndex visited) (Set.union moreToVisit extraToVisitMinusCurrent) (currentResults <> resultSet)

termUnitIdSet :: HieTypes.HieAST a -> Set Module.UnitId
termUnitIdSet ast =
  mconcat
    ( fmap
        identifierToUnitIdSet
        (Map.keys . HieTypes.nodeIdentifiers . HieTypes.nodeInfo $ ast)
    )
    <> mconcat (fmap termUnitIdSet (HieTypes.nodeChildren ast))

termModuleSet :: HieTypes.HieAST a -> Set Module.Module
termModuleSet ast =
  mconcat
    ( fmap
        identifierToModuleSet
        (Map.keys . HieTypes.nodeIdentifiers . HieTypes.nodeInfo $ ast)
    )
    <> mconcat (fmap termModuleSet (HieTypes.nodeChildren ast))

identifierToModuleSet :: Either a Name.Name -> Set Module.Module
identifierToModuleSet (Left _moduleName) = Set.empty -- Is this just a local module or do we need to resolve this?
identifierToModuleSet (Right name) = (maybeToSet . Name.nameModule_maybe) name

identifierToUnitIdSet :: Either a Name.Name -> Set Module.UnitId
identifierToUnitIdSet (Left _moduleName) = Set.empty
identifierToUnitIdSet (Right name) = nameToUnitIdSet name

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
  (Text.pack . Yiul.Directory.removeDotDirectories . FastString.unpackFS . SrcLoc.srcLocFile) loc
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

data Package
  = PackageUnitId Module.UnitId
  | PackageExe PackageExeId -- HIE files don't have unambiguous names for exes (and tests) so we approximate by the folder of the hie file (only works if hie files are in the build output dirs; if all hie files are in the same dir, then this approach won't work)
  deriving (Eq, Ord, Show)

-- | Makes a single directory for a given package, possibly long name but without slashes, etc.
makePackageDirectory :: Package -> PackageName
makePackageDirectory (PackageUnitId unitId) = mkConst $ Text.pack ("lib-" <> (FastString.unpackFS . Module.unitIdFS) unitId)
makePackageDirectory (PackageExe path) = mkConst $ Text.pack ("exe-" <> (alphaNumericDashPath . unConst) path)

isGoodPathChar :: Char -> Bool
isGoodPathChar c = Char.isAlphaNum c || c == '-' || c == '_'

alphaNumericDashPath :: FilePath -> FilePath
alphaNumericDashPath [] = []
alphaNumericDashPath (c : rest) | isGoodPathChar c = c : alphaNumericDashPath rest
alphaNumericDashPath (_ : rest) = '_' : alphaNumericDashPath (dropWhile (not . isGoodPathChar) rest)

-- | Makes a single directory for a fully qualified module name, could also work as a file name.
-- @Data.Something.Else@ becomes @Data-Something-Else@
makeModuleDirectory :: Module.ModuleName -> FilePath
makeModuleDirectory = fmap (\c -> if c == '.' then '-' else c) . Module.moduleNameString

makePackage :: (HieFilePath, HieFileResult) -> Package
makePackage (hieFilePath, hieFileResult) =
  let hieModule = HieTypes.hie_module . HieBin.hie_file_result $ hieFileResult
      unitId = Module.moduleUnitId hieModule
   in if unitId /= Module.mainUnitId
        then PackageUnitId unitId
        else
          let moduleName = Module.moduleNameString . Module.moduleName $ hieModule
              moduleNameDepth = (+ 1) . length . filter (== '.') $ moduleName
              iterateTakeDirectory n path | n > 0 = iterateTakeDirectory (n - 1) (FilePath.takeDirectory path)
              iterateTakeDirectory _ path = path
              -- assume for module name like 'Data.Something.Other' that the .hie file is in a directory like 'some/dir/Data/Something/Other.hie'
              -- the result would be 'PackageExe "some/dir"'
              basePath = iterateTakeDirectory moduleNameDepth (unConst hieFilePath)
              stackTweaks = (Yiul.Directory.removeFinalNestedTmp . Yiul.Directory.removeStackWorkThroughBuild) basePath
           in PackageExe (mkConst stackTweaks)

organizeByPackages :: [(HieFilePath, HieFileResult)] -> IO (Map Package [(HieFilePath, HieFileResult)])
organizeByPackages inputPairs = do
  let result = Map.fromListWith (<>) $ fmap (\x -> (makePackage x, [x])) inputPairs
  putStrLn $ "Loaded " <> (show . length . Map.keys) result <> " packages with a total of " <> (show . length . concat . Map.elems) result <> " modules."
  pure result

makePackagesReport :: Map Package [(HieFilePath, HieFileResult)] -> Text
makePackagesReport = makeTsv . (headerLine :) . concatMap handlePair . Map.assocs
  where
    headerLine =
      ( "Package",
        "Module",
        "Package Directory",
        "Module Directory",
        "Names count"
      )
    handlePair (package, pairs) =
      fmap
        ( \pair ->
            let nameSet = (getAllNamesForFile . HieBin.hie_file_result . snd) pair
             in ( (Text.pack . show) package,
                  (Text.pack . Module.moduleNameString . Module.moduleName . HieTypes.hie_module . HieBin.hie_file_result . snd) pair,
                  (unConst . makePackageDirectory) package,
                  (Text.pack . makeModuleDirectory . Module.moduleName . HieTypes.hie_module . HieBin.hie_file_result . snd) pair,
                  (Text.pack . show . Set.size) nameSet
                )
        )
        pairs

-- | Returns all names (visible and not visible) within the full type.
-- Probably inefficient when done per index since it may do redundant work for other type indexes.
getNameSetFromType :: HieTypes.HieFile -> HieTypes.TypeIndex -> Set Name.Name
getNameSetFromType hieFile typeIndex = collectFromTypeIndexSet collectNamesForHieType VisibleAndInvsibleArgs hieFile (Set.singleton typeIndex)

getNameSetForAstExcludingChildren :: HieTypes.HieFile -> HieTypes.HieAST HieTypes.TypeIndex -> Set Name.Name
getNameSetForAstExcludingChildren hieFile ast =
  let nodeTypeNames = (Set.unions . fmap (getNameSetFromType hieFile) . HieTypes.nodeType . HieTypes.nodeInfo) ast
      identifierNames (Left _moduleName) = Set.empty -- when do we encounter this? do we need this for Name references?
      identifierNames (Right name) = Set.singleton name
      identifierDetailsNames = maybe Set.empty (getNameSetFromType hieFile) . HieTypes.identType
      identifierPairNames (identifier, details) = Set.union (identifierNames identifier) (identifierDetailsNames details)
      allIdentifierNames = (Set.unions . fmap identifierPairNames . Map.assocs . HieTypes.nodeIdentifiers . HieTypes.nodeInfo) ast
   in Set.union nodeTypeNames allIdentifierNames

getAllNamesForFile :: HieTypes.HieFile -> Set Name.Name
getAllNamesForFile hieFile =
  let astsMap = (HieTypes.getAsts . HieTypes.hie_asts) hieFile
      allAsts = concatMap HieUtils.flattenAst (Map.elems astsMap)
      nameSets = fmap (getNameSetForAstExcludingChildren hieFile) allAsts
   in Set.unions nameSets

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, a ~ a8, a ~ a9, a ~ a10, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6, b ~ b7, b ~ b8, b ~ b9, b ~ b10) => Lens.Each (a, a2, a3, a4, a5, a6, a7, a8, a9, a10) (b, b2, b3, b4, b5, b6, b7, b8, b9, b10) a b where
  each f ~(a, b, c, d, e, g, h, i, j, k) = (,,,,,,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g <*> f h <*> f i <*> f j <*> f k
  {-# INLINE each #-}

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, a ~ a8, a ~ a9, a ~ a10, a ~ a11, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6, b ~ b7, b ~ b8, b ~ b9, b ~ b10, b ~ b11) => Lens.Each (a, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) (b, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11) a b where
  each f ~(a, b, c, d, e, g, h, i, j, k, l) = (,,,,,,,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g <*> f h <*> f i <*> f j <*> f k <*> f l
  {-# INLINE each #-}

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, a ~ a8, a ~ a9, a ~ a10, a ~ a11, a ~ a12, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6, b ~ b7, b ~ b8, b ~ b9, b ~ b10, b ~ b11, b ~ b12) => Lens.Each (a, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) (b, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12) a b where
  each f ~(a, b, c, d, e, g, h, i, j, k, l, m) = (,,,,,,,,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g <*> f h <*> f i <*> f j <*> f k <*> f l <*> f m
  {-# INLINE each #-}

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, a ~ a8, a ~ a9, a ~ a10, a ~ a11, a ~ a12, a ~ a13, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6, b ~ b7, b ~ b8, b ~ b9, b ~ b10, b ~ b11, b ~ b12, b ~ b13) => Lens.Each (a, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) (b, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13) a b where
  each f ~(a, b, c, d, e, g, h, i, j, k, l, m, n) = (,,,,,,,,,,,,) <$> f a <*> f b <*> f c <*> f d <*> f e <*> f g <*> f h <*> f i <*> f j <*> f k <*> f l <*> f m <*> f n
  {-# INLINE each #-}
