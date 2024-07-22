module Arkham.Asset.Cards.Stealth3 (stealth3, Stealth3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Evade
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest (getSkillTestSource, getSkillTestTarget)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.SkillTestResult

newtype Stealth3 = Stealth3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stealth3 :: AssetCard Stealth3
stealth3 = asset Stealth3 Cards.stealth3

instance HasAbilities Stealth3 where
  getAbilities (Stealth3 attrs) =
    [controlledAbility attrs 1 (DuringTurn You) $ FastAbility' (exhaust attrs) [#evade]]

instance HasModifiersFor Stealth3 where
  getModifiersFor (EnemyTarget eid) (Stealth3 attrs) = maybeModified attrs do
    EnemyTarget eid' <- MaybeT getSkillTestTarget
    guard $ eid' == eid
    source <- MaybeT getSkillTestSource
    guard $ isAbilitySource attrs 1 source
    pure [EnemyEvade (-2)]
  getModifiersFor _ _ = pure []

instance RunMessage Stealth3 where
  runMessage msg a@(Stealth3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      pushM $ setTarget attrs <$> mkChooseEvade sid iid (attrs.ability 1)
      pure a
    AfterSkillTestEnds (isAbilitySource attrs 1 -> True) target@(EnemyTarget eid) (SucceededBy _ _) -> do
      for_ attrs.controller \iid -> do
        canDisengage <- iid <=~> InvestigatorCanDisengage
        isYourTurn <- iid <=~> TurnInvestigator
        when isYourTurn $ turnModifier iid attrs target (EnemyCannotEngage iid)
        pushWhen canDisengage $ DisengageEnemy iid eid
      pure a
    _ -> Stealth3 <$> liftRunMessage msg attrs
