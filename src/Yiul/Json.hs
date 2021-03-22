{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Yiul.Json where

import qualified Avail
import qualified BasicTypes
import Data.Aeson (ToJSON (toJSON), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Array as Array
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as Text.Encoding
import qualified FastString
import qualified FieldLabel
import GHC.Generics (Generic)
import qualified HieBin
import qualified HieTypes
import qualified IfaceType
import qualified Module
import qualified Name
import qualified OccName
import qualified SrcLoc
import qualified Unique
import qualified Var
import Yiul.Const hiding (ModuleName)
import Prelude hiding (span)

newtype HieFileList = HieFileList [(HieFilePath, HieBin.HieFileResult)]
  deriving stock (Generic)

instance ToJSON HieFileList where
  toJSON (HieFileList list) = toJSON (fmap HieFilePair list)

newtype HieFilePair = HieFilePair (HieFilePath, HieBin.HieFileResult)

instance ToJSON HieFilePair where
  toJSON
    ( HieFilePair
        ( hieFilePath,
          hieFileResult
          )
      ) =
      Aeson.object
        [ "hieFilePath" .= hieFilePath,
          "hieFileResult" .= HieFileResult hieFileResult
        ]

newtype HieFileResult = HieFileResult HieBin.HieFileResult

instance ToJSON HieFileResult where
  toJSON
    ( HieFileResult
        HieBin.HieFileResult
          { hie_file_result_version,
            hie_file_result_ghc_version,
            hie_file_result
          }
      ) =
      Aeson.object
        [ "hie_file_result_version" .= hie_file_result_version,
          "hie_file_result_ghc_version" .= Text.Encoding.decodeUtf8 hie_file_result_ghc_version,
          "hie_file_result" .= HieFile hie_file_result
        ]

newtype HieFile = HieFile HieTypes.HieFile

instance ToJSON HieFile where
  toJSON
    ( HieFile
        HieTypes.HieFile
          { hie_hs_file,
            hie_module,
            hie_types,
            hie_asts,
            hie_exports,
            hie_hs_src
          }
      ) =
      Aeson.object
        [ "hie_hs_file" .= hie_hs_file,
          "hie_module" .= Module hie_module,
          "hie_types" .= fmap HieTypeFlat (Array.elems hie_types),
          "hie_asts" .= HieASTs hie_asts,
          "hie_exports" .= fmap AvailInfo hie_exports,
          "hie_hs_src" .= Text.Encoding.decodeUtf8 hie_hs_src
        ]

newtype HieASTs = HieASTs (HieTypes.HieASTs HieTypes.TypeIndex)

instance ToJSON HieASTs where
  toJSON (HieASTs (HieTypes.HieASTs hieMap)) =
    toJSON (Map.fromList . fmap (\(k, v) -> (FastString.unpackFS k, HieAST v)) . Map.toList $ hieMap)

newtype HieAST = HieAST (HieTypes.HieAST HieTypes.TypeIndex)

instance ToJSON HieAST where
  toJSON (HieAST (HieTypes.Node {nodeInfo, nodeSpan, nodeChildren})) =
    Aeson.object
      [ "nodeInfo" .= NodeInfo nodeInfo,
        "nodeSpan" .= RealSrcSpan nodeSpan,
        "nodeChildren" .= fmap HieAST nodeChildren
      ]

newtype NodeInfo = NodeInfo (HieTypes.NodeInfo HieTypes.TypeIndex)

instance ToJSON NodeInfo where
  toJSON (NodeInfo (HieTypes.NodeInfo {nodeAnnotations, nodeType, nodeIdentifiers})) =
    Aeson.object
      [ "nodeAnnotations" .= fmap NodeAnnotation (Set.toList nodeAnnotations),
        "nodeType" .= nodeType,
        "nodeIdentifiers" .= fmap IdentifierPair (Map.toList nodeIdentifiers)
      ]

newtype NodeAnnotation = NodeAnnotation (FastString.FastString, FastString.FastString)

instance ToJSON NodeAnnotation where
  toJSON (NodeAnnotation (nodeConstructor, nodeType)) =
    Aeson.object
      [ "nodeConstructor" .= FastString.unpackFS nodeConstructor,
        "nodeType" .= FastString.unpackFS nodeType
      ]

newtype IdentifierPair = IdentifierPair (HieTypes.Identifier, HieTypes.IdentifierDetails HieTypes.TypeIndex)

instance ToJSON IdentifierPair where
  toJSON (IdentifierPair (identifier, details)) =
    Aeson.object
      [ "identifier" .= Identifier identifier,
        "details" .= IdentifierDetails details
      ]

newtype IdentifierDetails = IdentifierDetails (HieTypes.IdentifierDetails HieTypes.TypeIndex)

instance ToJSON IdentifierDetails where
  toJSON (IdentifierDetails (HieTypes.IdentifierDetails {identType, identInfo})) =
    Aeson.object
      [ "identType" .= identType,
        "identInfo" .= fmap ContextInfo (Set.toList identInfo)
      ]

newtype ContextInfo = ContextInfo HieTypes.ContextInfo

instance ToJSON ContextInfo where
  toJSON (ContextInfo HieTypes.Use) =
    Aeson.object
      [ "tag" .= str "Use"
      ]
  toJSON (ContextInfo HieTypes.MatchBind) =
    Aeson.object
      [ "tag" .= str "MatchBind"
      ]
  toJSON (ContextInfo (HieTypes.IEThing ieType)) =
    Aeson.object
      [ "tag" .= str "IEThing",
        "ieType" .= IEType ieType
      ]
  toJSON (ContextInfo HieTypes.TyDecl) =
    Aeson.object
      [ "tag" .= str "TyDecl"
      ]
  toJSON (ContextInfo (HieTypes.ValBind bindType scope span)) =
    Aeson.object
      [ "tag" .= str "ValBind",
        "bindType" .= BindType bindType,
        "scope" .= Scope scope,
        "span" .= fmap RealSrcSpan span
      ]
  toJSON (ContextInfo (HieTypes.PatternBind scopeInPattern scopeOutsidePattern span)) =
    Aeson.object
      [ "tag" .= str "PatternBind",
        "scopeInPattern" .= Scope scopeInPattern,
        "scopeOutsidePattern" .= Scope scopeOutsidePattern,
        "span" .= fmap RealSrcSpan span
      ]
  toJSON (ContextInfo (HieTypes.ClassTyDecl span)) =
    Aeson.object
      [ "tag" .= str "ClassTyDecl",
        "span" .= fmap RealSrcSpan span
      ]
  toJSON (ContextInfo (HieTypes.Decl declType span)) =
    Aeson.object
      [ "tag" .= str "Decl",
        "declType" .= DeclType declType,
        "span" .= fmap RealSrcSpan span
      ]
  toJSON (ContextInfo (HieTypes.TyVarBind scope tyVarScope)) =
    Aeson.object
      [ "tag" .= str "TyVarBind",
        "scope" .= Scope scope,
        "tyVarScope" .= TyVarScope tyVarScope
      ]
  toJSON (ContextInfo (HieTypes.RecField recFieldContext span)) =
    Aeson.object
      [ "tag" .= str "RecField",
        "recFieldContext" .= RecFieldContext recFieldContext,
        "span" .= fmap RealSrcSpan span
      ]

newtype RecFieldContext = RecFieldContext HieTypes.RecFieldContext

instance ToJSON RecFieldContext where
  toJSON (RecFieldContext HieTypes.RecFieldDecl) = str "RecFieldDecl"
  toJSON (RecFieldContext HieTypes.RecFieldAssign) = str "RecFieldAssign"
  toJSON (RecFieldContext HieTypes.RecFieldMatch) = str "RecFieldMatch"
  toJSON (RecFieldContext HieTypes.RecFieldOcc) = str "RecFieldOcc"

newtype TyVarScope = TyVarScope HieTypes.TyVarScope

instance ToJSON TyVarScope where
  toJSON (TyVarScope (HieTypes.ResolvedScopes scopes)) =
    Aeson.object
      [ "tag" .= str "ResolvedScopes",
        "scopes" .= fmap Scope scopes
      ]
  toJSON (TyVarScope (HieTypes.UnresolvedScope names span)) =
    Aeson.object
      [ "tag" .= str "UnresolvedScope",
        "names" .= fmap Name names,
        "span" .= fmap RealSrcSpan span
      ]

newtype DeclType = DeclType HieTypes.DeclType

instance ToJSON DeclType where
  toJSON (DeclType HieTypes.FamDec) = "FamDec"
  toJSON (DeclType HieTypes.SynDec) = "SynDec"
  toJSON (DeclType HieTypes.DataDec) = "DataDec"
  toJSON (DeclType HieTypes.ConDec) = "ConDec"
  toJSON (DeclType HieTypes.PatSynDec) = "PatSynDec"
  toJSON (DeclType HieTypes.ClassDec) = "ClassDec"
  toJSON (DeclType HieTypes.InstDec) = "InstDec"

newtype BindType = BindType HieTypes.BindType

instance ToJSON BindType where
  toJSON (BindType HieTypes.RegularBind) = str "RegularBind"
  toJSON (BindType HieTypes.InstanceBind) = str "InstanceBind"

newtype Scope = Scope HieTypes.Scope

instance ToJSON Scope where
  toJSON (Scope HieTypes.NoScope) =
    Aeson.object
      [ "tag" .= str "NoScope"
      ]
  toJSON (Scope (HieTypes.LocalScope span)) =
    Aeson.object
      [ "tag" .= str "LocalScope",
        "span" .= RealSrcSpan span
      ]
  toJSON (Scope HieTypes.ModuleScope) =
    Aeson.object
      [ "tag" .= str "ModuleScope"
      ]

newtype IEType = IEType HieTypes.IEType

instance ToJSON IEType where
  toJSON (IEType HieTypes.Import) = "Import"
  toJSON (IEType HieTypes.ImportAs) = "ImportAs"
  toJSON (IEType HieTypes.ImportHiding) = "ImportHiding"
  toJSON (IEType HieTypes.Export) = "Export"

newtype Identifier = Identifier (HieTypes.Identifier)

instance ToJSON Identifier where
  toJSON (Identifier (Left moduleName)) =
    Aeson.object
      [ "tag" .= str "ModuleName",
        "moduleName" .= ModuleName moduleName
      ]
  toJSON (Identifier (Right name)) =
    Aeson.object
      [ "tag" .= str "Name",
        "name" .= Name name
      ]

newtype AvailInfo = AvailInfo Avail.AvailInfo

instance ToJSON AvailInfo where
  toJSON (AvailInfo (Avail.Avail name)) =
    Aeson.object
      [ "tag" .= str "Avail",
        "name" .= Name name
      ]
  toJSON (AvailInfo (Avail.AvailTC name pieces fields)) =
    Aeson.object
      [ "tag" .= str "Avail",
        "name" .= Name name,
        "pieces" .= fmap Name pieces,
        "fields" .= fmap FieldLabel fields
      ]

newtype FieldLabel = FieldLabel FieldLabel.FieldLabel

instance ToJSON FieldLabel where
  toJSON (FieldLabel (FieldLabel.FieldLabel {flLabel, flIsOverloaded, flSelector})) =
    Aeson.object
      [ "flLabel" .= FastString.unpackFS flLabel,
        "flIsOverloaded" .= flIsOverloaded,
        "flSelector" .= Name flSelector
      ]

newtype Module = Module Module.Module

instance ToJSON Module where
  toJSON (Module (Module.Module unitId moduleName)) =
    Aeson.object ["unitId" .= UnitId unitId, "moduleName" .= ModuleName moduleName]

newtype UnitId = UnitId Module.UnitId

instance ToJSON UnitId where
  toJSON (UnitId unitId) = toJSON (Module.unitIdString unitId)

newtype ModuleName = ModuleName Module.ModuleName

instance ToJSON ModuleName where
  toJSON (ModuleName moduleName) = toJSON (Module.moduleNameString moduleName)

newtype HieTypeFlat = HieTypeFlat (HieTypes.HieType HieTypes.TypeIndex)

instance ToJSON HieTypeFlat where
  toJSON (HieTypeFlat (HieTypes.HTyVarTy name)) =
    Aeson.object
      [ "tag" .= str "HTyVarTy",
        "name" .= Name name
      ]
  toJSON (HieTypeFlat (HieTypes.HAppTy index args)) =
    Aeson.object
      [ "tag" .= str "HAppTy",
        "typeIndex" .= index,
        "args" .= HieArgs args
      ]
  toJSON (HieTypeFlat (HieTypes.HTyConApp ifaceTyCon args)) =
    Aeson.object
      [ "tag" .= str "HTyConApp",
        "ifaceTyCon" .= IfaceTyCon ifaceTyCon,
        "args" .= HieArgs args
      ]
  toJSON (HieTypeFlat (HieTypes.HForAllTy ((name, index1), argFlag) index2)) =
    Aeson.object
      [ "tag" .= str "HForAllTy",
        "name" .= Name name,
        "typeIndex1" .= index1,
        "argFlag" .= ArgFlag argFlag,
        "typeIndex2" .= index2
      ]
  toJSON (HieTypeFlat (HieTypes.HFunTy index1 index2)) =
    Aeson.object
      [ "tag" .= str "HFunTy",
        "typeIndex1" .= index1,
        "typeIndex2" .= index2
      ]
  toJSON (HieTypeFlat (HieTypes.HQualTy index1 index2)) =
    Aeson.object
      [ "tag" .= str "HQualTy",
        "typeIndex1" .= index1,
        "typeIndex2" .= index2
      ]
  toJSON (HieTypeFlat (HieTypes.HLitTy ifaceTyLit)) =
    Aeson.object
      [ "tag" .= str "HLitTy",
        "ifaceTyLit" .= IfaceTyLit ifaceTyLit
      ]
  toJSON (HieTypeFlat (HieTypes.HCastTy index)) =
    Aeson.object
      [ "tag" .= str "HCastTy",
        "typeIndex" .= index
      ]
  toJSON (HieTypeFlat HieTypes.HCoercionTy) =
    Aeson.object
      [ "tag" .= str "HCoercionTy"
      ]

str :: Text -> Aeson.Value
str = Aeson.String

newtype IfaceTyLit = IfaceTyLit IfaceType.IfaceTyLit

instance ToJSON IfaceTyLit where
  toJSON (IfaceTyLit (IfaceType.IfaceNumTyLit num)) =
    Aeson.object
      [ "tag" .= str "IfaceNumTyLit",
        "num" .= num
      ]
  toJSON (IfaceTyLit (IfaceType.IfaceStrTyLit strValue)) =
    Aeson.object
      [ "tag" .= str "IfaceStrTyLit",
        "str" .= FastString.unpackFS strValue
      ]

newtype ArgFlag = ArgFlag Var.ArgFlag

instance ToJSON ArgFlag where
  toJSON (ArgFlag (Var.Inferred)) = str "Inferred"
  toJSON (ArgFlag (Var.Specified)) = str "Specified"
  toJSON (ArgFlag (Var.Required)) = str "Required"

newtype IfaceTyCon = IfaceTyCon IfaceType.IfaceTyCon

instance ToJSON IfaceTyCon where
  toJSON (IfaceTyCon IfaceType.IfaceTyCon {ifaceTyConName, ifaceTyConInfo}) =
    Aeson.object
      [ "ifaceTyConName" .= Name ifaceTyConName,
        "ifaceTyConInfo" .= IfaceTyConInfo ifaceTyConInfo
      ]

newtype IfaceTyConInfo = IfaceTyConInfo IfaceType.IfaceTyConInfo

instance ToJSON IfaceTyConInfo where
  toJSON (IfaceTyConInfo IfaceType.IfaceTyConInfo {ifaceTyConIsPromoted, ifaceTyConSort}) =
    Aeson.object
      [ "ifaceTyConIsPromoted" .= PromotionFlag ifaceTyConIsPromoted,
        "ifaceTyConSort" .= IfaceTyConSort ifaceTyConSort
      ]

newtype IfaceTyConSort = IfaceTyConSort IfaceType.IfaceTyConSort

instance ToJSON IfaceTyConSort where
  toJSON (IfaceTyConSort IfaceType.IfaceNormalTyCon) =
    Aeson.object
      [ "tag" .= str "IfaceNormalTyCon"
      ]
  toJSON (IfaceTyConSort (IfaceType.IfaceTupleTyCon arity tupleSort)) =
    Aeson.object
      [ "tag" .= str "IfaceTupleTyCon",
        "arity" .= arity,
        "tupleSort" .= TupleSort tupleSort
      ]
  toJSON (IfaceTyConSort (IfaceType.IfaceSumTyCon arity)) =
    Aeson.object
      [ "tag" .= str "IfaceSumTyCon",
        "arity" .= arity
      ]
  toJSON (IfaceTyConSort IfaceType.IfaceEqualityTyCon) =
    Aeson.object
      [ "tag" .= str "IfaceEqualityTyCon"
      ]

newtype TupleSort = TupleSort BasicTypes.TupleSort

instance ToJSON TupleSort where
  toJSON (TupleSort BasicTypes.BoxedTuple) = str "BoxedTuple"
  toJSON (TupleSort BasicTypes.UnboxedTuple) = str "UnboxedTuple"
  toJSON (TupleSort BasicTypes.ConstraintTuple) = str "ConstraintTuple"

newtype PromotionFlag = PromotionFlag BasicTypes.PromotionFlag

instance ToJSON PromotionFlag where
  toJSON (PromotionFlag BasicTypes.NotPromoted) = str "NotPromoted"
  toJSON (PromotionFlag BasicTypes.IsPromoted) = str "IsPromoted"

newtype HieArgs = HieArgs (HieTypes.HieArgs HieTypes.TypeIndex)

instance ToJSON HieArgs where
  toJSON (HieArgs (HieTypes.HieArgs pairs)) = toJSON (fmap HieArgPair pairs)

newtype HieArgPair = HieArgPair (Bool, HieTypes.TypeIndex)

instance ToJSON HieArgPair where
  toJSON (HieArgPair (visible, index)) =
    Aeson.object
      [ "visible" .= visible,
        "index" .= index
      ]

newtype Name = Name Name.Name

instance ToJSON Name where
  toJSON (Name name) = case HieBin.toHieName name of
    HieBin.ExternalName moduleValue occName srcSpan ->
      Aeson.object
        [ "tag" .= str "ExternalName",
          "module" .= Module moduleValue,
          "occName" .= OccName occName,
          "srcSpan" .= SrcSpan srcSpan
        ]
    HieBin.LocalName occName srcSpan ->
      Aeson.object
        [ "tag" .= str "LocalName",
          "occName" .= OccName occName,
          "srcSpan" .= SrcSpan srcSpan
        ]
    HieBin.KnownKeyName unique ->
      Aeson.object
        [ "tag" .= str "KnownKeyName",
          "unique" .= Unique.getKey unique
        ]

newtype OccName = OccName OccName.OccName

instance ToJSON OccName where
  toJSON (OccName occName) =
    Aeson.object
      [ "nameSpace" .= NameSpace (OccName.occNameSpace occName),
        "occNameString" .= OccName.occNameString occName
      ]

newtype SrcSpan = SrcSpan SrcLoc.SrcSpan

newtype NameSpace = NameSpace OccName.NameSpace

instance ToJSON NameSpace where
  toJSON (NameSpace nameSpace) | nameSpace == OccName.varName = str "VarName"
  toJSON (NameSpace nameSpace) | nameSpace == OccName.dataName = str "DataName"
  toJSON (NameSpace nameSpace) | nameSpace == OccName.tvName = str "TvName"
  toJSON (NameSpace nameSpace) | nameSpace == OccName.tcClsName = str "TcClsName"
  toJSON (NameSpace _) = str "UnknownNameSpace"

instance ToJSON SrcSpan where
  toJSON (SrcSpan (SrcLoc.RealSrcSpan realSrcSpan)) =
    Aeson.object
      [ "tag" .= str "RealSrcSpan",
        "span" .= RealSrcSpan realSrcSpan
      ]
  toJSON (SrcSpan (SrcLoc.UnhelpfulSpan unhelpfulSpan)) =
    Aeson.object
      [ "tag" .= str "UnhelpfulSpan",
        "span" .= FastString.unpackFS unhelpfulSpan
      ]

newtype RealSrcSpan = RealSrcSpan SrcLoc.RealSrcSpan

instance ToJSON RealSrcSpan where
  toJSON (RealSrcSpan span) =
    Aeson.object
      [ "file" .= (FastString.unpackFS . SrcLoc.srcSpanFile) span,
        "startLine" .= SrcLoc.srcSpanStartLine span,
        "endLine" .= SrcLoc.srcSpanEndLine span,
        "startCol" .= SrcLoc.srcSpanStartCol span,
        "endCol" .= SrcLoc.srcSpanEndCol span
      ]
