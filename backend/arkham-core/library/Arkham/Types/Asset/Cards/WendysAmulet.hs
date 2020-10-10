{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.WendysAmulet where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype WendysAmulet = WendysAmulet Attrs
  deriving newtype (Show, ToJSON, FromJSON)

wendysAmulet :: AssetId -> WendysAmulet
wendysAmulet uuid =
  WendysAmulet $ (baseAttrs uuid "01014") { assetSlots = [AccessorySlot] }

instance IsInvestigator investigator => HasModifiersFor env investigator WendysAmulet where
  getModifiersFor _ i (WendysAmulet a) =
    pure [ CanPlayTopOfDiscard (Just EventType, []) | ownedBy a i ]

instance HasActions env investigator WendysAmulet where
  getActions i window (WendysAmulet x) = getActions i window x

instance (AssetRunner env) => RunMessage env WendysAmulet where
  runMessage msg (WendysAmulet attrs) = WendysAmulet <$> runMessage msg attrs
