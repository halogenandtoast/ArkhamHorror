{-# LANGUAGE QuantifiedConstraints #-}
module Arkham.Field where

import Arkham.Prelude

data family Field a :: Type -> Type

instance (Eq (Field a typ), Show (Field a typ)) => Hashable (Field a typ) where
  hashWithSalt s = hashWithSalt s . show

instance Show (Field a typ) => Eq (Field a typ) where
  a == b = show a == show b

data SomeField a where
  SomeField :: (ToJSON typ, FromJSON typ, Hashable typ, Typeable typ, Show typ, Eq typ) => Field a typ -> SomeField a
