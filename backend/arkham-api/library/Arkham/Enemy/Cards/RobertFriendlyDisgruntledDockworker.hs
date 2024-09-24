module Arkham.Enemy.Cards.RobertFriendlyDisgruntledDockworker (
  robertFriendlyDisgruntledDockworker,
  RobertFriendlyDisgruntledDockworker (..),
)
where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Id
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype RobertFriendlyDisgruntledDockworker = RobertFriendlyDisgruntledDockworker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

robertFriendlyDisgruntledDockworker :: EnemyCard RobertFriendlyDisgruntledDockworker
robertFriendlyDisgruntledDockworker =
  enemy
    RobertFriendlyDisgruntledDockworker
    Cards.robertFriendlyDisgruntledDockworker
    (4, Static 4, 1)
    (2, 1)

instance HasAbilities RobertFriendlyDisgruntledDockworker where
  getAbilities (RobertFriendlyDisgruntledDockworker a) =
    extend
      a
      [skillTestAbility $ restrictedAbility a 1 OnSameLocation parleyAction_]

instance RunMessage RobertFriendlyDisgruntledDockworker where
  runMessage msg e@(RobertFriendlyDisgruntledDockworker attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeClues attrs attrs =<< perPlayer 1
      place attrs =<< selectJust (LocationWithTitle "Innsmouth Harbour")
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getId
      parley sid iid (attrs.ability 1) attrs #agility (Fixed 2)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      moveTokens (attrs.ability 1) attrs iid #clue 1
      doStep 2 msg
      pure e
    DoStep 2 (PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True)) -> do
      when (attrs.token #clue == 0) $ addToVictory attrs
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      pure e
    _ -> RobertFriendlyDisgruntledDockworker <$> liftRunMessage msg attrs
