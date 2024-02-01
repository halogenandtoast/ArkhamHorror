module Arkham.Asset.Cards.ValentinoRivas (
  valentinoRivas,
  ValentinoRivas (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype ValentinoRivas = ValentinoRivas AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

valentinoRivas :: AssetCard ValentinoRivas
valentinoRivas =
  allyWith ValentinoRivas Cards.valentinoRivas (2, 3) (isStoryL .~ True)

instance HasModifiersFor ValentinoRivas where
  getModifiersFor (InvestigatorTarget iid) (ValentinoRivas a) =
    pure [toModifier a (SkillModifier SkillAgility 1) | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance HasAbilities ValentinoRivas where
  getAbilities (ValentinoRivas x) =
    [ restrictedAbility x 1 (ControlsThis <> DuringSkillTest (YourSkillTest AnySkillTest))
        $ FastAbility
        $ exhaust x
        <> ResourceCost 2
    ]

instance RunMessage ValentinoRivas where
  runMessage msg a@(ValentinoRivas attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ skillTestModifier attrs SkillTestTarget (Difficulty (-1))
      pure a
    _ -> ValentinoRivas <$> runMessage msg attrs
