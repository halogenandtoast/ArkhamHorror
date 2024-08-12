{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Orphans where

import Arkham.Card.CardCode
import Arkham.Game
import Arkham.Message
import Control.Error.Util (hush)
import Control.Monad.Fail qualified as Fail
import Data.Aeson hiding (Key)
import Data.Aeson.Types hiding (Key)
import Data.ByteString.Char8 qualified as BS8
import Data.Hashable (Hashable (hash))
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql
import Relude
import Web.HttpApiData
import Web.PathPieces
import Yesod.Core.Content
import Yesod.Test (SIO)

fmapLeft :: (a -> b) -> Either a c -> Either b c
fmapLeft f (Left a) = Left (f a)
fmapLeft _ (Right a) = Right a -- Rewrap to fix types.

instance PersistFieldSql Game where
  sqlType _ = SqlString

instance PersistField Game where
  toPersistValue = toPersistValue . toJSON
  fromPersistValue val =
    fromPersistValue val >>= fmapLeft T.pack . parseEither parseJSON

instance PersistFieldSql [Message] where
  sqlType _ = SqlString

instance PersistField [Message] where
  toPersistValue = toPersistValue . toJSON
  fromPersistValue val =
    fromPersistValue val >>= fmapLeft T.pack . parseEither parseJSON

instance PathPiece UUID where
  toPathPiece = toUrlPiece
  fromPathPiece = hush . parseUrlPiece

deriving newtype instance PathPiece CardCode

instance PersistField UUID where
  toPersistValue u = PersistLiteral_ Escaped . BS8.pack . UUID.toString $ u
  fromPersistValue (PersistLiteral_ _ t) =
    case UUID.fromString $ BS8.unpack t of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

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
  toContent = toContent . toEncoding

instance ToBackendKey SqlBackend a => ToJSONKey (Key a) where
  toJSONKey = toJSONKeyText keyAsText
   where
    keyAsText = T.pack . show . fromSqlKey

instance ToBackendKey SqlBackend a => FromJSONKey (Key a) where
  fromJSONKey = toSqlKey <$> fromJSONKey

instance (Eq (Key a), ToBackendKey SqlBackend a) => Hashable (Key a) where
  hash = hash . fromSqlKey
  hashWithSalt n = hashWithSalt n . fromSqlKey

instance Fail.MonadFail (SIO s) where
  fail = liftIO . Fail.fail
