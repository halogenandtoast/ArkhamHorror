{-# LANGUAGE QuantifiedConstraints #-}
module Arkham.Cost.FieldCost where

import Arkham.Prelude

data FieldCost

instance ToJSON FieldCost
instance FromJSON FieldCost
instance Show FieldCost
instance Eq FieldCost
instance Ord FieldCost
