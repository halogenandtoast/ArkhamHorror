module Arkham.Asset.Assets.Stealth (stealth) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillTestResult
import Arkham.Window qualified as Window

newtype Stealth = Stealth AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stealth :: AssetCard Stealth
stealth = asset Stealth Cards.stealth

instance HasAbilities Stealth where
  getAbilities (Stealth attrs) = [evadeAbility attrs 1 (ActionCost 1 <> exhaust attrs) ControlsThis]

instance RunMessage Stealth where
  runMessage msg a@(Stealth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseEvadeEnemyEdit sid iid (attrs.ability 1) (setTarget attrs)
      pure a
    ChosenEvadeEnemy sid source@(isSource attrs -> True) eid -> do
      skillTestModifier sid source eid (EnemyEvade (-2))
      pure a
    AfterSkillTestEnds
      (isAbilitySource attrs 1 -> True)
      (ProxyTarget (EnemyTarget eid) _)
      (SucceededBy {}) -> do
        for_ attrs.controller \iid -> do
          whenMatch iid TurnInvestigator $ turnModifier iid attrs eid (CannotEngage iid)
          checkWhen $ Window.EnemyEvaded iid eid
          whenMatch iid InvestigatorCanDisengage do
            disengageEnemy iid eid
            enemyCheckEngagement eid
          checkAfter $ Window.EnemyEvaded iid eid
        pure a
    _ -> Stealth <$> liftRunMessage msg attrs
