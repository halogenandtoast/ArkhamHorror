{-# LANGUAGE QuantifiedConstraints #-}
module Arkham.Query where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Field
import Arkham.Location.Types

data QueryG a where
  And :: [QueryG a] -> QueryG a
  At :: QueryG Location -> QueryG a
  In :: Element typ -> Field a typ -> QueryG a
  FieldEq :: Field a typ -> typ -> QueryG a

(==.) :: Field a typ -> typ -> QueryG a
(==.) = FieldEq
infix 4 ==.

selectCountG :: (HasGame m, Monad m, Queryable a) => QueryG a -> m Int
selectCountG q = length <$> selectG q

selectAnyG :: (HasGame m, Monad m, Queryable a) => QueryG a -> m Bool
selectAnyG q = notNull <$> selectG q

class Queryable a where
  selectG :: (HasGame m, Monad m) => QueryG a -> m [a]

instance (FieldDict ToJSON a, FieldDict ToJSON Location, forall typ. ToJSON (Field a typ), forall typ. ToJSON (Field Location typ)) => ToJSON (QueryG a) where
  toJSON = \case
    In x fld -> withElemFieldDict @ToJSON fld $
      object ["tag" .= String "in", "field" .= toJSON fld, "value" .= toJSON x]
    And qs ->
      object ["tag" .= String "and", "contents" .= toJSON qs]
    At locQ ->
      object ["tag" .= String "at", "contents" .= toJSON locQ]
    FieldEq fld v -> withFieldDict @ToJSON fld $
      object ["tag" .= String "eq", "field" .= toJSON fld, "value" .= toJSON v]

instance (FieldDict FromJSON a, FieldDict FromJSON Location, Record a) => FromJSON (QueryG a) where
  parseJSON = withObject "QueryG" $ \v -> do
    tag :: Text <- v .: "tag"
    case tag of
      "in" -> do
        sfld <- v .: "field"
        case sfld of
          (SomeField (fld :: Field a typ)) -> do
            withElemFieldDict @FromJSON fld $ do
              val <- v .: "value"
              pure $ In val fld
      "eq" -> do
        sfld <- v .: "field"
        case sfld of
          (SomeField (fld :: Field a typ)) -> do
            withFieldDict @FromJSON fld $ do
              val <- v .: "value"
              pure $ FieldEq fld val
      "and" -> do
        qs <- v .: "contents"
        pure $ And qs
      "at" -> do
        q <- v .: "contents"
        pure $ At q
      _ -> error "invalid parse"
