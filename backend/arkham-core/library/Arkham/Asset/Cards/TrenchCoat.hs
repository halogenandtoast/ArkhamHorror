module Arkham.Asset.Cards.TrenchCoat (
  trenchCoat,
  TrenchCoat (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.SkillType

newtype TrenchCoat = TrenchCoat AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trenchCoat :: AssetCard TrenchCoat
trenchCoat = assetWith TrenchCoat Cards.trenchCoat (healthL ?~ 2)

instance HasModifiersFor TrenchCoat where
  getModifiersFor (InvestigatorTarget iid) (TrenchCoat a) =
    pure
      [ toModifier a $ ActionSkillModifier Action.Evade SkillAgility 1
      | controlledBy a iid
      ]
  getModifiersFor _ _ = pure []

instance RunMessage TrenchCoat where
  runMessage msg (TrenchCoat attrs) = TrenchCoat <$> runMessage msg attrs
