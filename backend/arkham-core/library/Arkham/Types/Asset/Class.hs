module Arkham.Types.Asset.Class
  ( module Arkham.Types.Asset.Class
  )
where

import Arkham.Import
import Arkham.Types.Asset.Uses

class IsAsset a where
  slotsOf :: a -> [SlotType]
  useTypeOf :: a -> Maybe UseType
  isHealthDamageable :: a -> Bool
  isSanityDamageable :: a -> Bool
  isStory :: a -> Bool
