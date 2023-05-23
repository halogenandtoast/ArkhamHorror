module Entity.Arkham.ArkhamDBDecklist where

import Relude

import Arkham.Card
import Arkham.Decklist
import Arkham.Id
import Data.Aeson.Types
import Data.Text qualified as T
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql

fmapLeft :: (a -> b) -> Either a c -> Either b c
fmapLeft f (Left a) = Left (f a)
fmapLeft _ (Right a) = Right a -- Rewrap to fix types.

instance PersistFieldSql ArkhamDBDecklist where
  sqlType _ = SqlString

instance PersistField ArkhamDBDecklist where
  toPersistValue = toPersistValue . toJSON
  fromPersistValue val =
    fromPersistValue val >>= fmapLeft T.pack . parseEither parseJSON
