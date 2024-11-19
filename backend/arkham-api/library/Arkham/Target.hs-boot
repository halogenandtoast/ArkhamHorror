module Arkham.Target where

import Arkham.Prelude

data Target

instance Data Target
instance ToJSON Target
instance FromJSON Target
instance Eq Target
instance Ord Target
instance Show Target

class Targetable a where
  toTarget :: a -> Target
  isTarget :: a -> Target -> Bool
  isTarget = (==) . toTarget
  {-# MINIMAL toTarget #-}
