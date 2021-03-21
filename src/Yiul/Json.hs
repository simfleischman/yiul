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
import Yiul.Const

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
