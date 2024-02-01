{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Slot (module X, module Arkham.Slot) where

import Arkham.Prelude

import Arkham.Id
import Arkham.Matcher
import Arkham.SlotType as X
import Arkham.Source
import Arkham.Trait (Trait)

pattern TraitRestrictedSlot :: Source -> Trait -> [AssetId] -> Slot
pattern TraitRestrictedSlot source trait assetIds <- RestrictedSlot source (CardWithTrait trait) assetIds
  where
    TraitRestrictedSlot source trait assetIds = RestrictedSlot source (CardWithTrait trait) assetIds

data Slot
  = Slot {source :: Source, assets :: [AssetId]}
  | RestrictedSlot {source :: Source, matcher :: CardMatcher, assets :: [AssetId]}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks)

slotSource :: Slot -> Source
slotSource (Slot source _) = source
slotSource (RestrictedSlot source _ _) = source

instance Eq Slot where
  RestrictedSlot {} == RestrictedSlot {} = True
  Slot {} == Slot {} = True
  _ == _ = False

-- We want slots sorted by most restrictive, so assets take up the best match
instance Ord Slot where
  RestrictedSlot {} <= _ = True
  Slot {} <= Slot {} = True
  Slot {} <= _ = False
