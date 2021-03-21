{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Yiul.Json where

import Data.Aeson (ToJSON (toJSON), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Array as Array
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified FastString
import GHC.Generics (Generic)
import qualified HieBin
import qualified HieTypes
import qualified Module
import qualified Name
import qualified OccName
import qualified SrcLoc
import qualified Unique
import Yiul.Const hiding (ModuleName)

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
          "hie_file_result" .= () -- hie_file_result
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
          "hie_asts" .= (),
          "hie_exports" .= (),
          "hie_hs_src" .= ()
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
      [ "type" .= str "HTyVarTy",
        "name" .= Name name
      ]
  toJSON (HieTypeFlat (HieTypes.HAppTy index args)) =
    Aeson.object
      [ "type" .= str "HAppTy",
        "typeIndex" .= index,
        "args" .= ()
      ]
  toJSON (HieTypeFlat (HieTypes.HTyConApp ifaceTyCon args)) =
    Aeson.object
      [ "type" .= str "HTyConApp",
        "ifaceTyCon" .= (),
        "args" .= ()
      ]
  toJSON (HieTypeFlat (HieTypes.HForAllTy ((name, index1), argFlag) index2)) =
    Aeson.object
      [ "type" .= str "HForAllTy",
        "name" .= Name name,
        "index1" .= index1,
        "argFlag" .= (),
        "index2" .= index2
      ]
  toJSON (HieTypeFlat (HieTypes.HFunTy index1 index2)) =
    Aeson.object
      [ "type" .= str "HFunTy",
        "index1" .= index1,
        "index2" .= index2
      ]
  toJSON (HieTypeFlat (HieTypes.HQualTy index1 index2)) =
    Aeson.object
      [ "type" .= str "HQualTy",
        "index1" .= index1,
        "index2" .= index2
      ]
  toJSON (HieTypeFlat (HieTypes.HLitTy ifaceTyLit)) =
    Aeson.object
      [ "type" .= str "HLitTy",
        "ifaceTyLit" .= ()
      ]
  toJSON (HieTypeFlat (HieTypes.HCastTy index)) =
    Aeson.object
      [ "type" .= str "HCastTy",
        "index" .= index
      ]
  toJSON (HieTypeFlat HieTypes.HCoercionTy) =
    Aeson.object
      [ "type" .= str "HCoercionTy"
      ]

str :: Text -> Aeson.Value
str = Aeson.String

newtype Name = Name Name.Name

instance ToJSON Name where
  toJSON (Name name) = case HieBin.toHieName name of
    HieBin.ExternalName moduleValue occName srcSpan ->
      Aeson.object
        [ "type" .= str "ExternalName",
          "module" .= Module moduleValue,
          "occName" .= OccName occName,
          "srcSpan" .= SrcSpan srcSpan
        ]
    HieBin.LocalName occName srcSpan ->
      Aeson.object
        [ "type" .= str "LocalName",
          "occName" .= OccName occName,
          "srcSpan" .= SrcSpan srcSpan
        ]
    HieBin.KnownKeyName unique ->
      Aeson.object
        [ "type" .= str "KnownKeyName",
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
      [ "type" .= str "RealSrcSpan",
        "span" .= realSrcSpanToText realSrcSpan
      ]
  toJSON (SrcSpan (SrcLoc.UnhelpfulSpan unhelpfulSpan)) =
    Aeson.object
      [ "type" .= str "UnhelpfulSpan",
        "span" .= FastString.unpackFS unhelpfulSpan
      ]

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
