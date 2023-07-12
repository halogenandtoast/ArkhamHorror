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

data Update a where
  Update
    :: (Show typ, Eq typ, Typeable typ, FromJSON typ, ToJSON typ, Show (Field a typ), ToJSON (Field a typ))
    => Field a typ
    -> typ
    -> Update a

instance Show (Update a) where
  show (Update f v) = show f <> " = " <> show v

instance Eq (Update a) where
  Update (f1 :: Field a typ1) v1 == Update (f2 :: Field a typ2) v2 = case eqT @typ1 @typ2 of
    Just Refl -> f1 == f2 && v1 == v2
    Nothing -> False

instance ToJSON (Update a) where
  toJSON (Update f v) = object ["field" .= f, "value" .= v]

instance (FromJSON (SomeField a)) => FromJSON (Update a) where
  parseJSON = withObject "Update" $ \o -> do
    someField <- o .: "field"
    case someField of
      SomeField (f :: Field a typ) ->
        Update f <$> o .: "value"

(?=.)
  :: ( Show typ
     , Eq typ
     , Typeable typ
     , FromJSON typ
     , ToJSON typ
     , Show (Field a (Maybe typ))
     , ToJSON (Field a (Maybe typ))
     )
  => Field a (Maybe typ)
  -> typ
  -> Update a
(?=.) fld val = Update fld (Just val)

(=.)
  :: ( Show typ
     , Eq typ
     , Typeable typ
     , FromJSON typ
     , ToJSON typ
     , Show (Field a typ)
     , ToJSON (Field a typ)
     )
  => Field a typ
  -> typ
  -> Update a
(=.) = Update
