{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.HeirloomOfHyperborea where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Trait
import Arkham.Types.Window
import ClassyPrelude

newtype HeirloomOfHyperborea = HeirloomOfHyperborea Attrs
  deriving newtype (Show, ToJSON, FromJSON)

heirloomOfHyperborea :: AssetId -> HeirloomOfHyperborea
heirloomOfHyperborea uuid = HeirloomOfHyperborea
  $ (baseAttrs uuid "01012") { assetSlots = [AccessorySlot] }

instance HasModifiersFor env investigator HeirloomOfHyperborea where
  getModifiersFor _ _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator HeirloomOfHyperborea where
  getActions i (AfterPlayCard You traits) (HeirloomOfHyperborea Attrs {..})
    | Just (getId () i) == assetInvestigator = do
      if Spell `elem` traits
        then pure [DrawCards (getId () i) 1 False]
        else pure []
  getActions i window (HeirloomOfHyperborea x) = getActions i window x

instance (AssetRunner env) => RunMessage env HeirloomOfHyperborea where
  runMessage msg (HeirloomOfHyperborea attrs@Attrs {..}) =
    HeirloomOfHyperborea <$> runMessage msg attrs
