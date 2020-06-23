{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Entity.ArkhamGame where

import Arkham.Types.Game
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing
import Database.Persist.TH

mkPersist sqlSettings [persistLowerCase|
ArkhamGame sql=arkham_games
  currentData ArkhamGameData
  deriving Generic
|]

instance ToJSON ArkhamGame where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 10 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 10 }
