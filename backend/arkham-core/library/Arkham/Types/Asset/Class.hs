module Arkham.Types.Asset.Class
  ( module Arkham.Types.Asset.Class
  ) where

import Arkham.Prelude

import Arkham.Types.Asset.Uses
import Arkham.Types.Slot

class IsAsset a where
  slotsOf :: a -> [SlotType]
  useTypeOf :: a -> Maybe UseType
  isHealthDamageable :: a -> Bool
  isSanityDamageable :: a -> Bool
  isStory :: a -> Bool
