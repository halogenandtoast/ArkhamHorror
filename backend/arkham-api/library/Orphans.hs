{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans where

import Arkham.Types.GameJson
import ClassyPrelude hiding (fromString)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Data.UUID
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql
import Entity.Arkham.GameSetupState
import Yesod.Core.Content

fmapLeft :: (a -> b) -> Either a c -> Either b c
fmapLeft f (Left a) = Left (f a)
fmapLeft _ (Right a) = Right a -- Rewrap to fix types.

instance PersistFieldSql GameJson where
  sqlType _ = SqlString

instance PersistField GameJson where
  toPersistValue = toPersistValue . toJSON
  fromPersistValue val =
    fromPersistValue val >>= fmapLeft pack . parseEither parseJSON

instance PersistFieldSql GameSetupState where
  sqlType _ = SqlString

instance PersistField GameSetupState where
  toPersistValue = toPersistValue . toJSON
  fromPersistValue val =
    fromPersistValue val >>= fmapLeft pack . parseEither parseJSON

instance PersistFieldSql UUID where
  sqlType _ = SqlString

instance PersistField UUID where
  toPersistValue = toPersistValue . toString
  fromPersistValue val =
    fromPersistValue val >>= maybe (Left "invalid uuid") Right . fromString

-- Entity (and Key)
deriving stock instance Typeable Key
deriving stock instance Typeable Entity

instance {-# OVERLAPPABLE #-} (ToJSON a, PersistEntity a) => ToJSON (Entity a) where
  toJSON = entityIdToJSON

instance {-# OVERLAPPABLE #-} (FromJSON a, PersistEntity a) => FromJSON (Entity a) where
  parseJSON = entityIdFromJSON

instance {-# OVERLAPPABLE #-} ToJSON a => ToTypedContent a where
  toTypedContent = TypedContent typeJson . toContent

instance {-# OVERLAPPABLE #-} ToJSON a => ToContent a where
  toContent = toContent . encode

instance (ToBackendKey SqlBackend a) => ToJSONKey (Key a) where
  toJSONKey = toJSONKeyText keyAsText
    where keyAsText = T.pack . show . fromSqlKey

instance (ToBackendKey SqlBackend a) => FromJSONKey (Key a) where
  fromJSONKey = toSqlKey <$> fromJSONKey

instance (ToBackendKey SqlBackend a) => Hashable (Key a) where
  hash = hash . fromSqlKey
  hashWithSalt n = hashWithSalt n . fromSqlKey
