module Arkham.Asset.Assets.Stealth (stealth, Stealth (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Evade
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillTestResult
import Arkham.Window (mkAfter, mkWhen)
import Arkham.Window qualified as Window

newtype Stealth = Stealth AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stealth :: AssetCard Stealth
stealth = asset Stealth Cards.stealth

instance HasAbilities Stealth where
  getAbilities (Stealth attrs) = [evadeAbility attrs 1 (ActionCost 1 <> exhaust attrs) ControlsThis]

instance RunMessage Stealth where
  runMessage msg a@(Stealth attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      pushM $ setTarget attrs <$> mkChooseEvade sid iid (attrs.ability 1)
      pure a
    ChosenEvadeEnemy sid source@(isSource attrs -> True) eid -> do
      pushM $ skillTestModifier sid source eid (EnemyEvade (-2))
      pure a
    AfterSkillTestEnds
      (isAbilitySource attrs 1 -> True)
      (ProxyTarget (EnemyTarget eid) _)
      (SucceededBy {}) -> do
        let iid = getController attrs
        canDisengage <- iid <=~> InvestigatorCanDisengage
        isYourTurn <- iid <=~> TurnInvestigator
        whenWindow <- checkWindows [mkWhen $ Window.EnemyEvaded iid eid]
        afterWindow <- checkWindows [mkAfter $ Window.EnemyEvaded iid eid]
        enemyCannotEngage <- turnModifier iid attrs (toTarget eid) (EnemyCannotEngage iid)
        pushAll
          $ [enemyCannotEngage | isYourTurn]
          <> [whenWindow]
          <> [DisengageEnemy iid eid | canDisengage]
          <> [EnemyCheckEngagement eid | canDisengage]
          <> [afterWindow]
        pure a
    _ -> Stealth <$> runMessage msg attrs
