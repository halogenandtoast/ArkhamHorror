module Arkham.Homebrew.AgainstTheWendigo.Enemies.BernardEpstein (bernardEpstein) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype BernardEpstein = BernardEpstein EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bernardEpstein :: EnemyCard BernardEpstein
bernardEpstein = enemy BernardEpstein Cards.bernardEpstein & setPrey LowestRemainingHealth

instance HasModifiersFor BernardEpstein where
  -- "Bernard Epstein gains +1 [per_investigator] health."
  getModifiersFor (BernardEpstein a) = do
    n <- getPlayerCount
    modifySelf a [HealthModifier n]

instance HasAbilities BernardEpstein where
  getAbilities (BernardEpstein a) =
    [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage BernardEpstein where
  runMessage msg e@(BernardEpstein attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 5)
      pure e
    -- "If you succeed, put Bernard Epstein out of play."
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      removeFromGame attrs
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      pure e
    _ -> BernardEpstein <$> liftRunMessage msg attrs
