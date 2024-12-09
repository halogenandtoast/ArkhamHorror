module Arkham.Asset.Assets.ValentinoRivas (valentinoRivas, ValentinoRivas (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype ValentinoRivas = ValentinoRivas AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valentinoRivas :: AssetCard ValentinoRivas
valentinoRivas = allyWith ValentinoRivas Cards.valentinoRivas (2, 3) (isStoryL .~ True)

instance HasModifiersFor ValentinoRivas where
  getModifiersFor (ValentinoRivas a) = controllerGets a [SkillModifier #agility 1]

instance HasAbilities ValentinoRivas where
  getAbilities (ValentinoRivas x) =
    [ controlledAbility x 1 (DuringSkillTest $ YourSkillTest AnySkillTest)
        $ FastAbility
        $ exhaust x
        <> ResourceCost 2
    ]

instance RunMessage ValentinoRivas where
  runMessage msg a@(ValentinoRivas attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        pushM $ skillTestModifier sid attrs (SkillTestTarget sid) (Difficulty (-1))
      pure a
    _ -> ValentinoRivas <$> runMessage msg attrs
