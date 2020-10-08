{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ElderSignAmulet3 where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Slot
import ClassyPrelude

newtype ElderSignAmulet3 = ElderSignAmulet3 Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

elderSignAmulet3 :: AssetId -> ElderSignAmulet3
elderSignAmulet3 uuid = ElderSignAmulet3 $ (baseAttrs uuid "01095")
  { assetSlots = [AccessorySlot]
  , assetSanity = Just 4
  }

instance HasModifiersFor env investigator ElderSignAmulet3 where
  getModifiersFor _ _ _ = pure []

instance HasActions env investigator ElderSignAmulet3 where
  getActions i window (ElderSignAmulet3 x) = getActions i window x

instance (AssetRunner env) => RunMessage env ElderSignAmulet3 where
  runMessage msg (ElderSignAmulet3 attrs) =
    ElderSignAmulet3 <$> runMessage msg attrs
