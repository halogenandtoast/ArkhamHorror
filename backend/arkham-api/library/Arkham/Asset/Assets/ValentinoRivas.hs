module Arkham.Asset.Assets.ValentinoRivas (valentinoRivas) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher

newtype ValentinoRivas = ValentinoRivas AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valentinoRivas :: AssetCard ValentinoRivas
valentinoRivas = ally ValentinoRivas Cards.valentinoRivas (2, 3)

instance HasModifiersFor ValentinoRivas where
  getModifiersFor (ValentinoRivas a) = controllerGets a [SkillModifier #agility 1]

instance HasAbilities ValentinoRivas where
  getAbilities (ValentinoRivas x) =
    [ controlled x 1 (DuringSkillTest $ YourSkillTest AnySkillTest)
        $ FastAbility (exhaust x <> ResourceCost 2)
    ]

instance RunMessage ValentinoRivas where
  runMessage msg a@(ValentinoRivas attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid attrs sid (Difficulty (-1))
      pure a
    _ -> ValentinoRivas <$> liftRunMessage msg attrs
