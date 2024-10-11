module Arkham.Asset.Assets.Encyclopedia2 (Encyclopedia2 (..), encyclopedia2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.SkillType

newtype Encyclopedia2 = Encyclopedia2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

encyclopedia2 :: AssetCard Encyclopedia2
encyclopedia2 = asset Encyclopedia2 Cards.encyclopedia2

instance HasAbilities Encyclopedia2 where
  getAbilities (Encyclopedia2 a) = [restricted a 1 ControlsThis $ actionAbilityWithCost (exhaust a)]

instance RunMessage Encyclopedia2 where
  runMessage msg a@(Encyclopedia2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      investigators <- selectTargets $ affectsOthers $ colocatedWith iid
      chooseOneM iid do
        targets investigators \target ->
          chooseOneM iid do
            for_ labeledSkills \(label, skill) ->
              labeled label $ phaseModifier (attrs.ability 1) target $ SkillModifier skill 2
      pure a
    _ -> Encyclopedia2 <$> liftRunMessage msg attrs
