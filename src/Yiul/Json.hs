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
import qualified Data.Text.Encoding
import GHC.Generics (Generic)
import qualified HieBin
import qualified HieTypes
import qualified Module
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
          "hie_file_result_ghc_version" .= Data.Text.Encoding.decodeUtf8 hie_file_result_ghc_version,
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
          "hie_types" .= (),
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
