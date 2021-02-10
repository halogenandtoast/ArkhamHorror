module Arkham.Types.Asset.Cards.HolyRosary where

import Arkham.Prelude

import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Target
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype HolyRosary = HolyRosary AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

holyRosary :: AssetId -> HolyRosary
holyRosary uuid = HolyRosary $ (baseAttrs uuid "01059")
  { assetSlots = [AccessorySlot]
  , assetSanity = Just 2
  }

instance HasModifiersFor env  HolyRosary where
  getModifiersFor _ (InvestigatorTarget iid) (HolyRosary a) =
    pure [ toModifier a (SkillModifier SkillWillpower 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env HolyRosary where
  getActions i window (HolyRosary x) = getActions i window x

instance AssetRunner env => RunMessage env HolyRosary where
  runMessage msg (HolyRosary attrs) = HolyRosary <$> runMessage msg attrs
