module Arkham.Homebrew.DarkMatter.Enemies.CorruptedMachine (corruptedMachine) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype CorruptedMachine = CorruptedMachine EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- "Spawn - Location with the most clues."
corruptedMachine :: EnemyCard CorruptedMachine
corruptedMachine =
  enemy CorruptedMachine Cards.corruptedMachine & setSpawnAt (LocationWithMostClues Anywhere)

{- | "[action] Parley. Test [willpower] (3): If you succeed, discard Corrupted
Machine. If you fail, it immediately attacks you."
-}
instance HasAbilities CorruptedMachine where
  getAbilities (CorruptedMachine a) =
    extend1 a $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage CorruptedMachine where
  runMessage msg e@(CorruptedMachine attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> CorruptedMachine <$> liftRunMessage msg attrs
