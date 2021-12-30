module Arkham.Slot where

import Arkham.Prelude

import Arkham.AssetId
import Arkham.Source
import Arkham.Trait

data Slot = Slot Source (Maybe AssetId) | TraitRestrictedSlot Source Trait (Maybe AssetId)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Eq Slot where
  TraitRestrictedSlot{} == TraitRestrictedSlot{} = True
  Slot{} == Slot{} = True
  _ == _ = False

-- We want slots sorted by most restrictive, so assets take up the best match
instance Ord Slot where
  TraitRestrictedSlot{} <= _ = True
  Slot{} <= Slot{} = True
  Slot{} <= _ = False

newtype PotentialSlot = PotentialSlot { unPotentialSlot :: SlotType }

data SlotType
  = HandSlot
  | BodySlot
  | AllySlot
  | AccessorySlot
  | ArcaneSlot
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable, ToJSONKey, FromJSONKey)

isEmptySlot :: Slot -> Bool
isEmptySlot = isNothing . slotItem

canPutIntoSlot :: [Trait] -> Slot -> Bool
canPutIntoSlot traits = \case
  slot@Slot{} -> isEmptySlot slot
  tslot@(TraitRestrictedSlot _ t _) -> isEmptySlot tslot && t `elem` traits

putIntoSlot :: AssetId -> Slot -> Slot
putIntoSlot aid = \case
  Slot source _ -> Slot source (Just aid)
  TraitRestrictedSlot source t _ -> TraitRestrictedSlot source t (Just aid)

emptySlot :: Slot -> Slot
emptySlot = \case
  Slot source _ -> Slot source Nothing
  TraitRestrictedSlot source t _ -> TraitRestrictedSlot source t Nothing

slotItem :: Slot -> Maybe AssetId
slotItem = \case
  Slot _ masset -> masset
  TraitRestrictedSlot _ _ masset -> masset

removeIfMatches :: AssetId -> Slot -> Slot
removeIfMatches aid = \case
  Slot source masset ->
    if masset == Just aid then Slot source Nothing else Slot source masset
  TraitRestrictedSlot source trait masset -> if masset == Just aid
    then TraitRestrictedSlot source trait Nothing
    else TraitRestrictedSlot source trait masset
