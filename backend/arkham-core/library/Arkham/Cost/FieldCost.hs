{-# LANGUAGE QuantifiedConstraints #-}
module Arkham.Cost.FieldCost where

-- This module is a hot mess because we need to bring A LOT into scope in order
-- for this to be valid. Additionally we can't avoid the entity types being in
-- scope because hs-boot files can not have associated type families. In order
-- to circumvent this we have to have the Cost in an hs-boot file, but
-- unfortunately this causes a recompilation of Message, when it uses
-- TemplateHaskell, which has the worse compilation performance so Message
-- needed to be changed to Generic, which has exponential memory usage

import Arkham.Prelude

import Arkham.Classes.Entity
import Arkham.Field
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Projection
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Query
import Arkham.Classes.Query
import Data.Typeable

data FieldCost where
  FieldCost :: forall matcher rec. (Typeable rec, Typeable matcher, Typeable (Field rec Int), Show matcher, Show (Field rec Int), ToJSON matcher, ToJSON (Field rec Int), QueryElement matcher ~ EntityId rec, Hashable (Field rec Int), Hashable matcher, FromJSON (SomeField rec), FieldDict Typeable rec, Projection rec, Query matcher) => matcher -> Field rec Int -> FieldCost

deriving stock instance Show FieldCost

instance Eq FieldCost where
  (FieldCost (m1 :: m1) (f1 :: f1)) == (FieldCost (m2 :: m2) (f2 :: f2)) = case eqT @m1 @m2 of
    Just Refl -> case eqT @f1 @f2 of
      Just Refl -> m1 == m2 && f1 == f2
      Nothing -> False
    Nothing -> False

instance ToJSON FieldCost where
  toJSON (FieldCost matcher (fld :: Field rec typ)) = object ["matcher" .= matcher, "field" .= fld, "entity" .= show (typeRep $ Proxy @rec)]

instance FromJSON FieldCost where
  parseJSON = withObject "FieldCost" $ \v -> do
    entityType :: Text <- v .: "entity"
    case entityType of
      "Location" -> do
        sfld :: SomeField Location <- v .: "field"
        case sfld of
          SomeField (fld :: Field Location typ) -> withFieldDict @Typeable fld $ do
            case eqT @typ @Int of
              Just Refl -> do
                mtchr :: LocationMatcher <- v .: "matcher"
                pure $ FieldCost mtchr fld
              _ -> error "Must be an int"
      _ -> error "Unhandled"

instance Hashable FieldCost where
  hashWithSalt s (FieldCost x1 x2) = s + (hash x1) + (hash x2)
