module Arkham.Asset.Assets.TrenchCoat (trenchCoat) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Modifiers
import Arkham.Prelude

newtype TrenchCoat = TrenchCoat AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trenchCoat :: AssetCard TrenchCoat
trenchCoat = assetWith TrenchCoat Cards.trenchCoat (healthL ?~ 2)

instance HasModifiersFor TrenchCoat where
  getModifiersFor (TrenchCoat a) = controllerGets a [ActionSkillModifier #evade #agility 1]

instance RunMessage TrenchCoat where
  runMessage msg (TrenchCoat attrs) = TrenchCoat <$> runMessage msg attrs
