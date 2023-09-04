module Arkham.Asset.Cards.ValentinoRivas (
  valentinoRivas,
  ValentinoRivas (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.SkillType

newtype ValentinoRivas = ValentinoRivas AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valentinoRivas :: AssetCard ValentinoRivas
valentinoRivas =
  allyWith ValentinoRivas Cards.valentinoRivas (2, 3) (isStoryL .~ True)

instance HasModifiersFor ValentinoRivas where
  getModifiersFor (InvestigatorTarget iid) (ValentinoRivas a) =
    pure [toModifier a (SkillModifier SkillAgility 1) | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance RunMessage ValentinoRivas where
  runMessage msg (ValentinoRivas attrs) = ValentinoRivas <$> runMessage msg attrs
