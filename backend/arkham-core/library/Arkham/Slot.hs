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
  | AdjustableSlot
      {source :: Source, restriction :: Maybe CardMatcher, slotTypes :: [SlotType], assets :: [AssetId]}
  deriving stock (Show, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

slotSource :: Slot -> Source
slotSource (Slot source _) = source
slotSource (RestrictedSlot source _ _) = source
slotSource (AdjustableSlot source _ _ _) = source

isSlotSource :: Sourceable a => a -> Slot -> Bool
isSlotSource a slot = case (toSource a, slotSource slot) of
  (BothSource c d, BothSource e f) -> isSource c e || isSource c f || isSource d e || isSource d f
  (BothSource c d, ProxySource e f) -> isSource c e || isSource c f || isSource d e || isSource d f
  (BothSource c d, e) -> isSource c e || isSource d e
  (ProxySource c d, ProxySource e f) -> isSource c e || isSource c f || isSource d e || isSource d f
  (ProxySource c d, BothSource e f) -> isSource c e || isSource c f || isSource d e || isSource d f
  (ProxySource c d, e) -> isSource c e || isSource d e
  (e, BothSource c d) -> isSource c e || isSource d e
  (e, ProxySource c d) -> isSource c e || isSource d e
  (c, d) -> isSource c d

instance Eq Slot where
  RestrictedSlot {} == RestrictedSlot {} = True
  AdjustableSlot {} == AdjustableSlot {} = True
  Slot {} == Slot {} = True
  _ == _ = False

-- We want slots sorted by most restrictive, so assets take up the best match
instance Ord Slot where
  RestrictedSlot {} <= _ = True
  Slot {} <= Slot {} = True
  Slot {} <= AdjustableSlot {} = True
  Slot {} <= _ = False
  AdjustableSlot {} <= _ = True
