{-# LANGUAGE QuantifiedConstraints #-}

module Arkham.Cost.FieldCost where

import Arkham.Prelude

data FieldCost

instance NoThunks FieldCost
instance NFData FieldCost
instance Data FieldCost
instance ToJSON FieldCost
instance FromJSON FieldCost
instance Show FieldCost
instance Eq FieldCost
instance Ord FieldCost

data MaybeFieldCost

instance NoThunks MaybeFieldCost
instance NFData MaybeFieldCost
instance Data MaybeFieldCost
instance ToJSON MaybeFieldCost
instance FromJSON MaybeFieldCost
instance Show MaybeFieldCost
instance Eq MaybeFieldCost
instance Ord MaybeFieldCost
