module Arkham.Asset.Assets.Encyclopedia (Encyclopedia (..), encyclopedia) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.SkillType

newtype Encyclopedia = Encyclopedia AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

encyclopedia :: AssetCard Encyclopedia
encyclopedia = asset Encyclopedia Cards.encyclopedia

instance HasAbilities Encyclopedia where
  getAbilities (Encyclopedia a) =
    [ restricted a 1 ControlsThis $ actionAbilityWithCost $ exhaust a <> assetUseCost a Secret 1
    ]

instance RunMessage Encyclopedia where
  runMessage msg a@(Encyclopedia attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      investigators <- selectTargets =<< guardAffectsColocated iid
      chooseOneM iid do
        targets investigators \target -> do
          chooseOneM iid do
            for_ labeledSkills \(label, skill) ->
              labeled label $ phaseModifier source target (SkillModifier skill 2)
      pure a
    _ -> Encyclopedia <$> liftRunMessage msg attrs
