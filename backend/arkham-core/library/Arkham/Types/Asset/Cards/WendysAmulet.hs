{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.WendysAmulet where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude

newtype WendysAmulet = WendysAmulet Attrs
  deriving newtype (Show, ToJSON, FromJSON)

wendysAmulet :: AssetId -> WendysAmulet
wendysAmulet uuid =
  WendysAmulet $ (baseAttrs uuid "01014") { assetSlots = [AccessorySlot] }

instance HasActions env investigator WendysAmulet where
  getActions i window (WendysAmulet x) = getActions i window x

instance (AssetRunner env) => RunMessage env WendysAmulet where
  runMessage msg (WendysAmulet attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage
        (AddModifier
          (InvestigatorTarget iid)
          (AssetSource assetId)
          (CanPlayTopOfDiscard (Just EventType, []))
        )
      WendysAmulet <$> runMessage msg attrs
    _ -> WendysAmulet <$> runMessage msg attrs
