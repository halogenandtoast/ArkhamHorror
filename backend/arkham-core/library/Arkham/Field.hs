{-# LANGUAGE QuantifiedConstraints #-}

module Arkham.Field where

import Arkham.Prelude
import Data.Typeable

data family Field a :: Type -> Type

instance (Show (Field a typ)) => Eq (Field a typ) where
  a == b = show a == show b

data SomeField a where
  SomeField
    :: ( ToJSON typ
       , FromJSON typ
       , Ord typ
       , Typeable typ
       , Show typ
       , Eq typ
       , Show (Field a typ)
       , ToJSON (Field a typ)
       )
    => Field a typ
    -> SomeField a

instance Show (SomeField a) where
  show (SomeField f) = show f

instance Eq (SomeField a) where
  SomeField (f1 :: Field a typ1) == SomeField (f2 :: Field a typ2) = case eqT @typ1 @typ2 of
    Just Refl -> f1 == f2
    Nothing -> False

instance ToJSON (SomeField a) where
  toJSON (SomeField f) = toJSON f
