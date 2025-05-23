module Arkham.Enemy.Cards.JeremiahPierce (jeremiahPierce) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted

newtype JeremiahPierce = JeremiahPierce EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeremiahPierce :: EnemyCard JeremiahPierce
jeremiahPierce =
  enemyWith JeremiahPierce Cards.jeremiahPierce (4, Static 3, 4) (1, 1)
    $ spawnAtL
    ?~ SpawnAtFirst ["Your House", "Rivertown"]

instance HasAbilities JeremiahPierce where
  getAbilities (JeremiahPierce a) =
    extend a [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage JeremiahPierce where
  runMessage msg e@(JeremiahPierce attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addToVictory attrs
      sid <- getRandom
      parley sid iid attrs iid #willpower (Fixed 4)
      pure e
    FailedThisSkillTestBy _ (isSource attrs -> True) n -> do
      placeDoomOnAgendaAndCheckAdvance n
      pure e
    _ -> JeremiahPierce <$> liftRunMessage msg attrs
