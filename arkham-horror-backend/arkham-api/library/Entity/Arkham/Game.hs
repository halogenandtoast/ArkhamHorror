{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Entity.Arkham.Game
  ( ArkhamGame(..)
  , ArkhamGameId
  )
where

import Arkham.Types.GameJson
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing
import Data.List.NonEmpty (NonEmpty)
import Database.Persist.TH
import Lens.Micro
import Orphans ()

mkPersist sqlSettings [persistLowerCase|
ArkhamGame sql=arkham_games
  currentData GameJson
  deriving Generic Show
|]

instance ToJSON ArkhamGame where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 10 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 10 }
