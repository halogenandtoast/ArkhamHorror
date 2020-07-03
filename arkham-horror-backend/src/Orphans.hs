{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans where

import Data.Aeson
import Database.Persist.Sql
import Database.Persist.Types
import Prelude ((.))
import Yesod.Core.Content

instance {-# OVERLAPPABLE #-} ToJSON a => ToTypedContent a where
  toTypedContent = TypedContent typeJson . toContent

instance {-# OVERLAPPABLE #-} ToJSON a => ToContent a where
  toContent = toContent . encode

instance ToJSONKey (Key a) where
  toJSONKey = toJSONKey . fromSqlKey

