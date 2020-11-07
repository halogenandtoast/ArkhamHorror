{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.HeirloomOfHyperborea where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype HeirloomOfHyperborea = HeirloomOfHyperborea Attrs
  deriving newtype (Show, ToJSON, FromJSON)

heirloomOfHyperborea :: AssetId -> HeirloomOfHyperborea
heirloomOfHyperborea uuid =
  HeirloomOfHyperborea $ baseAttrs uuid "01012" $ slots .= [AccessorySlot]

instance HasModifiersFor env HeirloomOfHyperborea where
  getModifiersFor _ _ _ = pure []

instance HasActions env HeirloomOfHyperborea where
  getActions iid (AfterPlayCard You traits) (HeirloomOfHyperborea a)
    | ownedBy a iid = pure [ DrawCards iid 1 False | Spell `elem` traits ]
  getActions i window (HeirloomOfHyperborea x) = getActions i window x

instance (AssetRunner env) => RunMessage env HeirloomOfHyperborea where
  runMessage msg (HeirloomOfHyperborea attrs@Attrs {..}) =
    HeirloomOfHyperborea <$> runMessage msg attrs
