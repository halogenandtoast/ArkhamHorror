module Arkham.Slot (module X, module Arkham.Slot) where

import Arkham.Prelude

import Arkham.Id
import Arkham.Matcher
import Arkham.SlotType as X
import Arkham.Source
import Arkham.Trait (Trait)

pattern TraitRestrictedSlot :: Source -> Trait -> Maybe AssetId -> Slot
pattern TraitRestrictedSlot source trait massetId <- RestrictedSlot source (CardWithTrait trait) massetId
  where
    TraitRestrictedSlot source trait massetId = RestrictedSlot source (CardWithTrait trait) massetId

data Slot = Slot Source (Maybe AssetId) | RestrictedSlot Source CardMatcher (Maybe AssetId)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
