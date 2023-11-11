module Arkham.Cost.FieldCost where

import Arkham.Prelude

import Arkham.Classes.Entity
import Arkham.Classes.Query
import Arkham.Enemy.Types
import Arkham.Field
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Projection
import Arkham.Query
import Data.Data

data FieldCost where
  FieldCost
    :: forall matcher rec fld
     . ( fld ~ Field rec Int
       , QueryElement matcher ~ EntityId rec
       , Typeable matcher
       , Typeable rec
       , Typeable fld
       , Show matcher
       , Show fld
       , ToJSON matcher
       , ToJSON fld
       , Ord fld
       , Ord matcher
       , FromJSON (SomeField rec)
       , Projection rec
       , Query matcher
       )
    => matcher
    -> fld
    -> FieldCost

deriving stock instance Show FieldCost

instance Data FieldCost where
  gunfold _ _ _ = error "gunfold(FieldCost)"
  toConstr _ = error "toConstr(FieldCost)"
  dataTypeOf _ = error "dataTypeOf(FieldCost)"

instance Eq FieldCost where
  FieldCost (m1 :: m1) (f1 :: f1) == FieldCost (m2 :: m2) (f2 :: f2) = case eqT @m1 @m2 of
    Just Refl -> case eqT @f1 @f2 of
      Just Refl -> m1 == m2 && f1 == f2
      Nothing -> False
    Nothing -> False

instance ToJSON FieldCost where
  toJSON (FieldCost matcher (fld :: Field rec typ)) =
    object
      [ "matcher" .= matcher
      , "field" .= fld
      , "entity" .= show (typeRep $ Proxy @rec)
      ]

instance FromJSON FieldCost where
  parseJSON = withObject "FieldCost" $ \v -> do
    entityType :: Text <- v .: "entity"
    case entityType of
      "Location" -> do
        sfld :: SomeField Location <- v .: "field"
        case sfld of
          SomeField (fld :: Field Location typ) ->
            case eqT @typ @Int of
              Just Refl -> do
                mtchr :: LocationMatcher <- v .: "matcher"
                pure $ FieldCost mtchr fld
              _ -> error "Must be an int"
      _ -> error "Unhandled"

-- we do not care about this instance really
instance Ord FieldCost where
  compare f1 f2 = compare (show f1) (show f2)

data MaybeFieldCost where
  MaybeFieldCost
    :: forall matcher rec fld
     . ( fld ~ Field rec (Maybe Int)
       , QueryElement matcher ~ EntityId rec
       , Typeable matcher
       , Typeable rec
       , Typeable fld
       , Show matcher
       , Show fld
       , ToJSON matcher
       , ToJSON fld
       , Ord fld
       , Ord matcher
       , FromJSON (SomeField rec)
       , Projection rec
       , Query matcher
       )
    => matcher
    -> fld
    -> MaybeFieldCost

deriving stock instance Show MaybeFieldCost

instance Data MaybeFieldCost where
  gunfold _ _ _ = error "gunfold(MaybeFieldCost)"
  toConstr _ = error "toConstr(MaybeFieldCost)"
  dataTypeOf _ = error "dataTypeOf(MaybeFieldCost)"

instance Eq MaybeFieldCost where
  MaybeFieldCost (m1 :: m1) (f1 :: f1) == MaybeFieldCost (m2 :: m2) (f2 :: f2) = case eqT @m1 @m2 of
    Just Refl -> case eqT @f1 @f2 of
      Just Refl -> m1 == m2 && f1 == f2
      Nothing -> False
    Nothing -> False

instance ToJSON MaybeFieldCost where
  toJSON (MaybeFieldCost matcher (fld :: Field rec typ)) =
    object
      [ "matcher" .= matcher
      , "field" .= fld
      , "entity" .= show (typeRep $ Proxy @rec)
      ]

instance FromJSON MaybeFieldCost where
  parseJSON = withObject "MaybeFieldCost" $ \v -> do
    entityType :: Text <- v .: "entity"
    case entityType of
      "Enemy" -> do
        sfld :: SomeField Enemy <- v .: "field"
        case sfld of
          SomeField (fld :: Field Enemy typ) ->
            case eqT @typ @(Maybe Int) of
              Just Refl -> do
                mtchr :: EnemyMatcher <- v .: "matcher"
                pure $ MaybeFieldCost mtchr fld
              _ -> error "Must be a Maybe Int"
      _ -> error "Unhandled"

-- we do not care about this instance really
instance Ord MaybeFieldCost where
  compare f1 f2 = compare (show f1) (show f2)
