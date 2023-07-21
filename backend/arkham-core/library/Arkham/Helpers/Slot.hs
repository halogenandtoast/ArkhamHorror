module Arkham.Helpers.Slot (
  module Arkham.Helpers.Slot,
  module X,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Id
import Arkham.Slot as X

isEmptySlot :: Slot -> Bool
isEmptySlot = isNothing . slotItem

canPutIntoSlot :: IsCard a => a -> Slot -> Bool
canPutIntoSlot a = \case
  slot@Slot {} -> isEmptySlot slot
  tslot@(RestrictedSlot _ matcher _) -> isEmptySlot tslot && cardMatch a matcher

putIntoSlot :: AssetId -> Slot -> Slot
putIntoSlot aid = \case
  Slot source _ -> Slot source (Just aid)
  RestrictedSlot source t _ -> RestrictedSlot source t (Just aid)

emptySlot :: Slot -> Slot
emptySlot = \case
  Slot source _ -> Slot source Nothing
  RestrictedSlot source t _ -> RestrictedSlot source t Nothing

slotItem :: Slot -> Maybe AssetId
slotItem = \case
  Slot _ masset -> masset
  RestrictedSlot _ _ masset -> masset

removeIfMatches :: AssetId -> Slot -> Slot
removeIfMatches aid = \case
  Slot source masset ->
    if masset == Just aid then Slot source Nothing else Slot source masset
  RestrictedSlot source trait masset ->
    if masset == Just aid
      then RestrictedSlot source trait Nothing
      else RestrictedSlot source trait masset
