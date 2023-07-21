{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.Step (
  module Entity.Arkham.Step,
) where

import Relude

import Arkham.Message
import Data.Aeson.Diff
import Data.Aeson.Types
import Data.Text qualified as T
import Data.UUID
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql
import Database.Persist.TH
import Entity.Arkham.Game
import Json
import Orphans ()

data Choice = Choice
  { choicePatchDown :: Patch
  , choiceMessages :: [Message]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance PersistFieldSql Choice where
  sqlType _ = SqlString

instance PersistField Choice where
  toPersistValue = toPersistValue . toJSON
  fromPersistValue val =
    fromPersistValue val >>= fmapLeft T.pack . parseEither parseJSON
   where
    fmapLeft f (Left a) = Left (f a)
    fmapLeft _ (Right a) = Right a -- Rewrap to fix types.

share
  [mkPersist sqlSettings]
  [persistLowerCase|
ArkhamStep sql=arkham_steps
  Id UUID default=uuid_generate_v4()
  arkhamGameId ArkhamGameId OnDeleteCascade
  choice Choice
  step Int
  UniqueStep arkhamGameId step
  deriving Generic Show
|]

instance ToJSON ArkhamStep where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamStep"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamStep"

instance FromJSON ArkhamStep where
  parseJSON = genericParseJSON $ aesonOptions $ Just "arkhamStep"
