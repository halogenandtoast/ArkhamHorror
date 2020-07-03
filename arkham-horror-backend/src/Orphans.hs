{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans where

import Data.Aeson
import Data.Aeson.Types
import Database.Persist.Sql
import ClassyPrelude
import Yesod.Core.Content
import qualified Data.Text as T

instance {-# OVERLAPPABLE #-} ToJSON a => ToTypedContent a where
  toTypedContent = TypedContent typeJson . toContent

instance {-# OVERLAPPABLE #-} ToJSON a => ToContent a where
  toContent = toContent . encode

instance (ToBackendKey SqlBackend a) => ToJSONKey (Key a) where
  toJSONKey = toJSONKeyText keyAsText
    where
      keyAsText = T.pack . show . fromSqlKey

instance (ToBackendKey SqlBackend a) => FromJSONKey (Key a) where
  fromJSONKey = toSqlKey <$> fromJSONKey

instance (ToBackendKey SqlBackend a) => Hashable (Key a) where
  hash = hash . fromSqlKey
  hashWithSalt n = hashWithSalt n . fromSqlKey
