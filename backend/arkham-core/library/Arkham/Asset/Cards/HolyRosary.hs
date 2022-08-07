module Arkham.Asset.Cards.HolyRosary where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.SkillType
import Arkham.Target

newtype HolyRosary = HolyRosary AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

holyRosary :: AssetCard HolyRosary
holyRosary = assetWith HolyRosary Cards.holyRosary (sanityL ?~ 2)

instance HasModifiersFor  HolyRosary where
  getModifiersFor (InvestigatorTarget iid) (HolyRosary a) =
    pure [ toModifier a (SkillModifier SkillWillpower 1) | controlledBy a iid ]
  getModifiersFor _ _ = pure []

instance RunMessage HolyRosary where
  runMessage msg (HolyRosary attrs) = HolyRosary <$> runMessage msg attrs
