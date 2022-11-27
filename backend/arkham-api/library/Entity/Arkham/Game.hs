{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.Game (
  module Entity.Arkham.Game,
) where

import Relude

import Api.Arkham.Types.MultiplayerVariant
import Arkham.Game
import Arkham.Message
import Data.Aeson.Diff
import Data.Aeson.Types
import Data.Text qualified as T
import Data.Time.Clock
import Data.UUID
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql
import Database.Persist.TH
import Json
import Orphans ()

data Choice = Choice
  { choicePatchDown :: Patch
  , choiceMessages :: [Message]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance PersistFieldSql [Choice] where
  sqlType _ = SqlString

instance PersistField [Choice] where
  toPersistValue = toPersistValue . toJSON
  fromPersistValue val =
    fromPersistValue val >>= fmapLeft T.pack . parseEither parseJSON
   where
    fmapLeft f (Left a) = Left (f a)
    fmapLeft _ (Right a) = Right a -- Rewrap to fix types.

share
  [mkPersist sqlSettings]
  [persistLowerCase|
ArkhamGame sql=arkham_games
  Id UUID default=uuid_generate_v4()
  name Text
  currentData Game
  choices [Choice]
  log [Text]
  multiplayerVariant MultiplayerVariant
  createdAt UTCTime
  updatedAt UTCTime
  deriving Generic Show
|]

instance ToJSON ArkhamGame where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamGame"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamGame"
