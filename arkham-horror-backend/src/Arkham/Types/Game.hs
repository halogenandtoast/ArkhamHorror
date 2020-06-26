module Arkham.Types.Game where

import Arkham.Types.Difficulty
import Arkham.Types.GameState
import Arkham.Types.Scenario
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql

data ArkhamCycle = NightOfTheZealot | TheDunwichLegacy
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data ArkhamGameData = ArkhamGameData
  { agId :: Int
  , agCycle :: ArkhamCycle
  , agScenario :: ArkhamScenario
  , agDifficulty :: ArkhamDifficulty
  , agGameState :: ArkhamGameState
  }
  deriving stock (Generic, Show)

instance ToJSON ArkhamGameData where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }

instance FromJSON ArkhamGameData where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 2 }

fmapLeft :: (a -> b) -> Either a c -> Either b c
fmapLeft f (Left a) = Left (f a)
fmapLeft _ (Right a) = Right a -- Rewrap to fix types.

instance PersistFieldSql ArkhamGameData where
  sqlType _ = SqlString

instance PersistField ArkhamGameData where
  toPersistValue = toPersistValue . toJSON
  fromPersistValue val =
    fromPersistValue val >>= fmapLeft pack . parseEither parseJSON
