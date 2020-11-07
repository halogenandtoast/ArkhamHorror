{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.WendysAmulet where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype WendysAmulet = WendysAmulet Attrs
  deriving newtype (Show, ToJSON, FromJSON)

wendysAmulet :: AssetId -> WendysAmulet
wendysAmulet uuid =
  WendysAmulet $ baseAttrs uuid "01014" $ slots .= [AccessorySlot]

instance HasModifiersFor env WendysAmulet where
  getModifiersFor _ (InvestigatorTarget iid) (WendysAmulet a) =
    pure [ CanPlayTopOfDiscard (Just EventType, []) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env WendysAmulet where
  getActions i window (WendysAmulet x) = getActions i window x

instance (AssetRunner env) => RunMessage env WendysAmulet where
  runMessage msg (WendysAmulet attrs) = WendysAmulet <$> runMessage msg attrs
