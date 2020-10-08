{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.HolyRosary where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import ClassyPrelude

newtype HolyRosary = HolyRosary Attrs
  deriving newtype (Show, ToJSON, FromJSON)

holyRosary :: AssetId -> HolyRosary
holyRosary uuid = HolyRosary $ (baseAttrs uuid "01059")
  { assetSlots = [AccessorySlot]
  , assetSanity = Just 2
  }

instance IsInvestigator investigator => HasModifiersFor env investigator HolyRosary where
  getModifiersFor _ i (HolyRosary Attrs {..}) =
    pure
      [ SkillModifier SkillWillpower 1
      | Just (getId () i) == assetInvestigator
      ]

instance HasActions env investigator HolyRosary where
  getActions i window (HolyRosary x) = getActions i window x

instance (AssetRunner env) => RunMessage env HolyRosary where
  runMessage msg (HolyRosary attrs) = HolyRosary <$> runMessage msg attrs
