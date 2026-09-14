{-# OPTIONS_GHC -Wno-orphans #-}

-- | Stores a 'DeckOverlay' as JSON, the way a decklist is stored.
module Entity.Arkham.DeckOverlay where

import Arkham.Custom.Overlay
import Data.Aeson.Types
import Data.Text qualified as T
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql
import Relude

instance PersistFieldSql DeckOverlay where
  sqlType _ = SqlString

instance PersistField DeckOverlay where
  toPersistValue = toPersistValue . toJSON
  fromPersistValue val =
    fromPersistValue val >>= first T.pack . parseEither parseJSON
