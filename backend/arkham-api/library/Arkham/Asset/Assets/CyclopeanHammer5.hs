module Arkham.Asset.Assets.CyclopeanHammer5 (cyclopeanHammer5, CyclopeanHammer5 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestTarget, withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Taboo

newtype CyclopeanHammer5 = CyclopeanHammer5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cyclopeanHammer5 :: AssetCard CyclopeanHammer5
cyclopeanHammer5 = asset CyclopeanHammer5 Cards.cyclopeanHammer5

instance HasAbilities CyclopeanHammer5 where
  getAbilities (CyclopeanHammer5 a) = [restrictedAbility a 1 ControlsThis fightAction_]

instance RunMessage CyclopeanHammer5 where
  runMessage msg a@(CyclopeanHammer5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [DamageDealt 1, AddSkillValue #willpower]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      if tabooed TabooList20 attrs
        then when attrs.ready do
          chooseOrRunOneM iid do
            when (n >= 3) do
              labeled
                "Exhaust Cyclopean Hammer to instead deal +2 damage and move the enemy up to two locations away from you."
                do
                  exhaustThis attrs
                  doStep 1 msg
            labeled "Do not exhaust" do
              getSkillTestTarget >>= \case
                Just (EnemyTarget enemy) -> do
                  whenM (enemy <=~> NonEliteEnemy) do
                    choices <-
                      select
                        $ ConnectedFrom (locationWithInvestigator iid)
                        <> LocationCanBeEnteredBy enemy
                    when (notNull choices) do
                      chooseOneM iid do
                        labeled "Do not move enemy" nothing
                        labeled "Move enemy" do
                          chooseOneM iid do
                            questionLabeled "Move enemy away"
                            targets choices (push . EnemyMove enemy)
                _ -> nothing
        else doStep 1 msg
      pure a
    DoStep 1 (PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n) -> do
      getSkillTestTarget >>= \case
        Just (EnemyTarget enemy) -> do
          when (n >= 3) do
            withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
            whenM (enemy <=~> NonEliteEnemy) do
              choices <-
                select
                  $ LocationWithDistanceFromAtMost
                    (if n >= 3 then 2 else 1)
                    (locationWithInvestigator iid)
                    (LocationCanBeEnteredBy enemy <> not_ (locationWithInvestigator iid))
              when (notNull choices) do
                chooseOneM iid do
                  labeled "Do not move enemy" nothing
                  labeled "Move enemy" do
                    chooseOneM iid do
                      questionLabeled "Move enemy away"
                      targets choices (push . EnemyMove enemy)
        _ -> error "Something went wrong"
      pure a
    _ -> CyclopeanHammer5 <$> liftRunMessage msg attrs
