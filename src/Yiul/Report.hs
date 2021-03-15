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
import qualified IfaceType
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
          allTypeUnitIds = unitIdsForTypeIndex VisibleAndInvsibleArgs hieFile (typeIndexSetForAst ast)
          visibleTypeUnitIds = unitIdsForTypeIndex OnlyVisibleArgs hieFile (typeIndexSetForAst ast)
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

nameToUnitIdSet :: Name.Name -> Set Module.UnitId
nameToUnitIdSet = Set.fromList . fmap Module.moduleUnitId . Maybe.maybeToList . Name.nameModule_maybe

typeIndexSetForAst :: HieTypes.HieAST HieTypes.TypeIndex -> Set HieTypes.TypeIndex
typeIndexSetForAst ast =
  let nodeInfo = HieTypes.nodeInfo ast
      nodeTypeIndexSet = Set.fromList . HieTypes.nodeType $ nodeInfo
      identifierDetails :: Set HieTypes.TypeIndex
      identifierDetails = Set.fromList . Maybe.mapMaybe HieTypes.identType . Map.elems . HieTypes.nodeIdentifiers $ nodeInfo
      nodeChildren = HieTypes.nodeChildren ast
      childrenSets = typeIndexSetForAst <$> nodeChildren
   in nodeTypeIndexSet <> identifierDetails <> Set.unions childrenSets

data VisibleArgs = VisibleAndInvsibleArgs | OnlyVisibleArgs

hieArgsToIndexSet :: VisibleArgs -> HieTypes.HieArgs HieTypes.TypeIndex -> Set HieTypes.TypeIndex
hieArgsToIndexSet VisibleAndInvsibleArgs (HieTypes.HieArgs pairs) = Set.fromList (snd <$> pairs)
hieArgsToIndexSet OnlyVisibleArgs (HieTypes.HieArgs pairs) = Set.fromList (snd <$> filter fst pairs)

unitIdPairsForHieType :: VisibleArgs -> HieTypes.HieType HieTypes.TypeIndex -> (Set Module.UnitId, Set HieTypes.TypeIndex)
unitIdPairsForHieType _visibleArgs (HieTypes.HTyVarTy name) = (nameToUnitIdSet name, Set.empty)
unitIdPairsForHieType visibleArgs (HieTypes.HAppTy index1 args) = (Set.empty, Set.insert index1 (hieArgsToIndexSet visibleArgs args))
unitIdPairsForHieType visibleArgs (HieTypes.HTyConApp ifaceTyCon args) = ((nameToUnitIdSet . IfaceType.ifaceTyConName) ifaceTyCon, hieArgsToIndexSet visibleArgs args)
unitIdPairsForHieType _visibleArgs (HieTypes.HForAllTy ((name, index1), _argFlag) index2) = (nameToUnitIdSet name, Set.fromList [index1, index2])
unitIdPairsForHieType _visibleArgs (HieTypes.HFunTy index1 index2) = (Set.empty, Set.fromList [index1, index2])
unitIdPairsForHieType _visibleArgs (HieTypes.HQualTy index1 index2) = (Set.empty, Set.fromList [index1, index2])
unitIdPairsForHieType _visibleArgs (HieTypes.HLitTy _ifaceTyLit) = (Set.empty, Set.empty)
unitIdPairsForHieType _visibleArgs (HieTypes.HCastTy index) = (Set.empty, Set.singleton index)
unitIdPairsForHieType _visibleArgs HieTypes.HCoercionTy = (Set.empty, Set.empty)

unitIdsForTypeIndex :: VisibleArgs -> HieTypes.HieFile -> Set HieTypes.TypeIndex -> Set Module.UnitId
unitIdsForTypeIndex includeVisibleArgs hieFile typeIndexSet = go Set.empty typeIndexSet Set.empty
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
                      (currentResults, extraToVisit) = unitIdPairsForHieType includeVisibleArgs typ
                      extraToVisitMinusCurrent = Set.delete currentIndex extraToVisit
                   in go (Set.insert currentIndex visited) (Set.union moreToVisit extraToVisitMinusCurrent) (Set.union currentResults resultSet)

termUnitIdSet :: HieTypes.HieAST a -> Set Module.UnitId
termUnitIdSet ast =
  mconcat
    ( fmap
        identifierToUnitIdSet
        (Map.keys . HieTypes.nodeIdentifiers . HieTypes.nodeInfo $ ast)
    )
    <> mconcat (fmap termUnitIdSet (HieTypes.nodeChildren ast))

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
