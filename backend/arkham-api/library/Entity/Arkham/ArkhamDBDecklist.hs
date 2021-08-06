module Entity.Arkham.ArkhamDBDecklist where

import ClassyPrelude

import Arkham.Types.Card
import Arkham.Types.InvestigatorId
import Data.Aeson.Types
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql

fmapLeft :: (a -> b) -> Either a c -> Either b c
fmapLeft f (Left a) = Left (f a)
fmapLeft _ (Right a) = Right a -- Rewrap to fix types.

data ArkhamDBDecklist = ArkhamDBDecklist
  { slots :: HashMap CardCode Int
  , investigator_code :: InvestigatorId
  , investigator_name :: Text
  , meta :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance PersistFieldSql ArkhamDBDecklist where
  sqlType _ = SqlString

instance PersistField ArkhamDBDecklist where
  toPersistValue = toPersistValue . toJSON
  fromPersistValue val =
    fromPersistValue val >>= fmapLeft pack . parseEither parseJSON
