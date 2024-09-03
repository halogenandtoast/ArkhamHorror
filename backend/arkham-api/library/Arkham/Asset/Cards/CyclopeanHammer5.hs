module Arkham.Asset.Cards.CyclopeanHammer5 (cyclopeanHammer5, CyclopeanHammer5 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.SkillTest (getSkillTestTarget, withSkillTest)
import Arkham.Matcher
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
      pushM $ mkChooseFight sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTarget >>= \case
        Just (EnemyTarget enemy) -> whenM (enemy <=~> NonEliteEnemy) do
          if tabooed TabooList20 attrs
            then do
              when attrs.ready do
                chooseOne
                  iid
                  [ Label
                      "Exhaust Cyclopean Hammer to instead deal +2 damage and move the enemy up to two locations away from you."
                      [Exhaust (toTarget attrs), DoStep 1 msg]
                  , Label "Do not exhaust" []
                  ]
            else doStep 1 msg
        _ -> pure ()
      pure a
    DoStep 1 (PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n) -> do
      getSkillTestTarget >>= \case
        Just (EnemyTarget enemy) -> whenM (enemy <=~> NonEliteEnemy) do
          when (n >= 3) do
            withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
          choices <-
            select
              $ LocationWithDistanceFromAtMost
                (if n >= 3 then 2 else 1)
                (locationWithInvestigator iid)
                (LocationCanBeEnteredBy enemy <> not_ (locationWithInvestigator iid))
          when (notNull choices) do
            questionLabel "Move enemy away" iid
              $ ChooseOne
              $ targetLabels choices (only . EnemyMove enemy)
        _ -> error "Something went wrong"
      pure a
    _ -> CyclopeanHammer5 <$> liftRunMessage msg attrs
