{-# LANGUAGE QuantifiedConstraints #-}

module Arkham.Field where

import Arkham.Prelude
import Control.Monad.Fail (fail)
import Data.Data

data family Field a :: Type -> Type

instance Show (Field a typ) => Eq (Field a typ) where
  a == b = show a == show b

data SomeField a where
  SomeField
    :: ( ToJSON typ
       , FromJSON typ
       , Typeable typ
       , Typeable a
       , Show typ
       , Eq typ
       , Data a
       , Data (Field a typ)
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
  IncrementBy
    :: (Show (Field a Int), ToJSON (Field a Int))
    => Field a Int
    -> Int
    -> Update a
  DecrementBy
    :: (Show (Field a Int), ToJSON (Field a Int))
    => Field a Int
    -> Int
    -> Update a

instance Typeable a => Data (Update a) where
  gunfold _ _ _ = error "gunfold(Update a)"
  toConstr _ = error "toConstr(Update a)"
  dataTypeOf _ = error "dataTypeOf(Update a)"

instance Show (Update a) where
  show (Update f v) = show f <> " = " <> show v
  show (IncrementBy f v) = show f <> " + " <> show v
  show (DecrementBy f v) = show f <> " + " <> show v

instance Eq (Update a) where
  Update (f1 :: Field a typ1) v1 == Update (f2 :: Field a typ2) v2 = case eqT @typ1 @typ2 of
    Just Refl -> f1 == f2 && v1 == v2
    Nothing -> False
  Update {} == _ = False
  IncrementBy (f1 :: Field a Int) v1 == IncrementBy (f2 :: Field a Int) v2 =
    f1 == f2 && v1 == v2
  IncrementBy {} == _ = False
  DecrementBy (f1 :: Field a Int) v1 == IncrementBy (f2 :: Field a Int) v2 =
    f1 == f2 && v1 == v2
  DecrementBy {} == _ = False

instance ToJSON (Update a) where
  toJSON (Update f v) = object ["tag" .= String "update", "field" .= f, "value" .= v]
  toJSON (IncrementBy f v) = object ["tag" .= String "increment", "field" .= f, "value" .= v]
  toJSON (DecrementBy f v) = object ["tag" .= String "decrement", "field" .= f, "value" .= v]

instance FromJSON (SomeField a) => FromJSON (Update a) where
  parseJSON = withObject "Update" $ \o -> do
    tag :: String <- o .: "tag"
    case tag of
      "update" -> do
        someField <- o .: "field"
        case someField of
          SomeField (f :: Field a typ) ->
            Update f <$> o .: "value"
      "increment" -> do
        someField <- o .: "field"
        case someField of
          SomeField (f :: Field a typ) -> case eqT @typ @Int of
            Just Refl -> IncrementBy f <$> o .: "value"
            Nothing -> fail "Expected Int"
      "decrement" -> do
        someField <- o .: "field"
        case someField of
          SomeField (f :: Field a typ) -> case eqT @typ @Int of
            Just Refl -> DecrementBy f <$> o .: "value"
            Nothing -> fail "Expected Int"
      other -> fail $ "Expected update type: " <> other

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
