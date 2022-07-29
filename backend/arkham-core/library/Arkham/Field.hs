{-# LANGUAGE QuantifiedConstraints #-}
module Arkham.Field where

import Arkham.Prelude
import Data.Constraint
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Control.Monad (fail)

data SomeField r where
  SomeField :: Field r typ -> SomeField r

class Record r where
  data Field r :: Type -> Type
  fieldLookup :: Map Text (SomeField r)

withFieldDict :: forall c entity a r . FieldDict c entity => Field entity a -> (c a => r) ->r
withFieldDict l k = case getDict @c l of
  Dict -> k

withElemFieldDict :: forall c entity a b r . (b ~ Element a, FieldDict c entity) => Field entity a -> (c b => r) -> r
withElemFieldDict l k = case getElemDict @c l of
  Dict -> k

class FieldDict c entity where
  getDict :: Field entity a -> Dict (c a)
  getElemDict :: Element a ~ b => Field entity a -> Dict (c b)

instance (forall a. ToJSON (Field entity a)) => ToJSON (SomeField entity) where
  toJSON (SomeField field) = toJSON field

instance Record entity => FromJSON (SomeField entity) where
  parseJSON = withText "SomeField" $ \txt ->
    case Map.lookup txt fieldLookup of
      Nothing -> fail $ "Couldn't parse SomeField, got: " <> T.unpack txt
      Just fld -> pure fld
