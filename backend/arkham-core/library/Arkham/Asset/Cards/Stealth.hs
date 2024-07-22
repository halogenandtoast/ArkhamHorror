module Arkham.Asset.Cards.Stealth (stealth, Stealth (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Evade
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillTestResult

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
      withSkillTest \sid ->
        pushM $ setTarget attrs <$> mkChooseEvade sid iid (attrs.ability 1)
      pure a
    ChosenEvadeEnemy sid source@(isSource attrs -> True) eid -> do
      push $ skillTestModifier sid source eid (EnemyEvade (-2))
      pure a
    AfterSkillTestEnds (isSource attrs -> True) target@(EnemyTarget eid) (SucceededBy _ _) -> do
      let iid = getController attrs
      canDisengage <- iid <=~> InvestigatorCanDisengage
      isYourTurn <- iid <=~> TurnInvestigator
      pushAll
        $ [turnModifier iid attrs target (EnemyCannotEngage iid) | isYourTurn]
        <> [DisengageEnemy iid eid | canDisengage]
      pure a
    _ -> Stealth <$> runMessage msg attrs
