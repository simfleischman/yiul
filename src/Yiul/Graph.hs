{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Yiul.Graph where

import Data.Array ((!))
import qualified Data.Char as Char
import Data.Generics.Labels ()
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified FastString
import HieBin (HieFileResult)
import qualified HieBin
import qualified HieTypes
import qualified HieUtils
import qualified IfaceType
import qualified Module
import qualified Name
import qualified System.FilePath as FilePath
import qualified Topograph
import Yiul.Const
import qualified Yiul.Directory
import Prelude hiding (cycle, span)

data OurModule = OurModule
  { package :: Package,
    moduleName :: ModuleName
  }
  deriving (Eq, Ord, Show)

data Package
  = PackageUnitId Module.UnitId
  | PackageExe PackageExeId -- HIE files don't have unambiguous names for exes (and tests) so we approximate by the folder of the hie file (only works if hie files are in the build output dirs; if all hie files are in the same dir, then this approach won't work)
  deriving (Eq, Ord, Show)

buildModuleDependency :: Map Package [(HieFilePath, HieFileResult)] -> Map OurModule (Set OurModule)
buildModuleDependency =
  let getHieFileModule :: (Package, HieTypes.HieFile) -> OurModule
      getHieFileModule (pkg, hieFile) = convertModule pkg (HieTypes.hie_module hieFile)
      getDeps :: (Package, HieTypes.HieFile) -> Set OurModule
      getDeps (pkg, hieFile) =
        let names = getAllNamesForFile hieFile
         in Set.map (convertModule pkg) . Set.fromList . Maybe.catMaybes . fmap (Name.nameModule_maybe) . Set.toList $ names
      makeDep pair =
        let self = getHieFileModule pair
            deps = Set.delete self (getDeps pair)
         in (self, deps)
   in Map.fromList
        . fmap makeDep
        . concatMap (\(pkg, pairs) -> fmap (\pair -> (pkg, (HieBin.hie_file_result . snd) pair)) pairs)
        . Map.assocs

makeModuleDependencyClosure :: Map OurModule (Set OurModule) -> IO (Map OurModule (Set OurModule))
makeModuleDependencyClosure adjacencyMap = do
  let result = Topograph.runG adjacencyMap $ \g -> Topograph.adjacencyMap $ Topograph.closure g
  case result of
    Left cycle -> do
      putStrLn "Module cycle:"
      mapM_ print cycle
      fail "Module cycle found"
    Right depMap -> pure depMap

makeModuleReverseDependencyClosure :: Map OurModule (Set OurModule) -> IO (Map OurModule (Set OurModule))
makeModuleReverseDependencyClosure adjacencyMap = do
  let result = Topograph.runG adjacencyMap $ \g -> Topograph.adjacencyMap $ Topograph.transpose $ Topograph.closure g
  case result of
    Left cycle -> do
      putStrLn "Module cycle:"
      mapM_ print cycle
      fail "Module cycle found"
    Right depMap -> pure depMap

joinForwardAndReverseDependencies :: Map OurModule (Set OurModule) -> Map OurModule (Set OurModule) -> Map OurModule (Set OurModule, Set OurModule)
joinForwardAndReverseDependencies forwardDeps reverseDeps =
  Map.fromList
    . fmap
      ( \(m, fs) ->
          case Map.lookup m reverseDeps of
            Nothing -> (m, (fs, Set.empty))
            Just rs -> (m, (fs, rs))
      )
    . Map.assocs
    $ forwardDeps

getModuleName :: Module.Module -> ModuleName
getModuleName = mkConst @ModuleName . Text.pack . Module.moduleNameString . Module.moduleName

convertModule :: Package -> Module.Module -> OurModule
convertModule pkg moduleValue = do
  if Module.moduleUnitId moduleValue == Module.mainUnitId
    then
      OurModule
        { package = pkg,
          moduleName = getModuleName moduleValue
        }
    else
      OurModule
        { package = (PackageUnitId . Module.moduleUnitId) moduleValue,
          moduleName = getModuleName moduleValue
        }

-- | Makes a single directory for a given package, possibly long name but without slashes, etc.
makePackageDirectory :: Package -> FilePath
makePackageDirectory (PackageUnitId unitId) = "lib-" <> (FastString.unpackFS . Module.unitIdFS) unitId
makePackageDirectory (PackageExe path) = "exe-" <> (alphaNumericDashPath . unConst) path

isGoodPathChar :: Char -> Bool
isGoodPathChar c = Char.isAlphaNum c || c == '-' || c == '_'

alphaNumericDashPath :: FilePath -> FilePath
alphaNumericDashPath [] = []
alphaNumericDashPath (c : rest) | isGoodPathChar c = c : alphaNumericDashPath rest
alphaNumericDashPath (_ : rest) = '_' : alphaNumericDashPath (dropWhile (not . isGoodPathChar) rest)

makePackage :: (HieFilePath, HieFileResult) -> Package
makePackage (hieFilePath, hieFileResult) =
  let hieModule = HieTypes.hie_module . HieBin.hie_file_result $ hieFileResult
      unitId = Module.moduleUnitId hieModule
   in if unitId /= Module.mainUnitId
        then PackageUnitId unitId
        else
          let moduleNameString = Module.moduleNameString . Module.moduleName $ hieModule
              moduleNameDepth = (+ 1) . length . filter (== '.') $ moduleNameString
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

-- | Returns all names (visible and not visible) within the full type.
-- Probably inefficient when done per index since it may do redundant work for other type indexes.
getNameSetFromType :: HieTypes.HieFile -> HieTypes.TypeIndex -> Set Name.Name
getNameSetFromType hieFile typeIndex = collectFromTypeIndexSet collectNamesForHieType VisibleAndInvsibleArgs hieFile (Set.singleton typeIndex)

getNameSetForAstExcludingChildren :: HieTypes.HieFile -> HieTypes.HieAST HieTypes.TypeIndex -> Set Name.Name
getNameSetForAstExcludingChildren hieFile ast =
  let nodeTypeNames = (Set.unions . fmap (getNameSetFromType hieFile) . HieTypes.nodeType . HieTypes.nodeInfo) ast
      identifierNames (Left _moduleName) = Set.empty
      identifierNames (Right name) = Set.singleton name
      identifierDetailsNames = maybe Set.empty (getNameSetFromType hieFile) . HieTypes.identType
      identifierPairNames (identifier, details) = Set.union (identifierNames identifier) (identifierDetailsNames details)
      allIdentifierNames = (Set.unions . fmap identifierPairNames . Map.assocs . HieTypes.nodeIdentifiers . HieTypes.nodeInfo) ast
   in Set.union nodeTypeNames allIdentifierNames

getNameSetForAstRecursively :: HieTypes.HieFile -> HieTypes.HieAST HieTypes.TypeIndex -> Set Name.Name
getNameSetForAstRecursively hieFile ast =
  let nameSets = fmap (getNameSetForAstExcludingChildren hieFile) (HieUtils.flattenAst ast)
   in Set.unions nameSets

getAllNamesForFile :: HieTypes.HieFile -> Set Name.Name
getAllNamesForFile hieFile =
  let astsMap = (HieTypes.getAsts . HieTypes.hie_asts) hieFile
      allAsts = concatMap HieUtils.flattenAst (Map.elems astsMap)
      nameSets = fmap (getNameSetForAstExcludingChildren hieFile) allAsts
   in Set.unions nameSets

data VisibleArgs = VisibleAndInvsibleArgs | OnlyVisibleArgs | OnlyInvisibleArgs

hieArgsToIndexSet :: VisibleArgs -> HieTypes.HieArgs HieTypes.TypeIndex -> Set HieTypes.TypeIndex
hieArgsToIndexSet VisibleAndInvsibleArgs (HieTypes.HieArgs pairs) = Set.fromList (snd <$> pairs)
hieArgsToIndexSet OnlyVisibleArgs (HieTypes.HieArgs pairs) = Set.fromList (snd <$> filter fst pairs)
hieArgsToIndexSet OnlyInvisibleArgs (HieTypes.HieArgs pairs) = Set.fromList (snd <$> filter (not . fst) pairs)

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
