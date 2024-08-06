module Arkham.Enemy.Cards.AccursedFollower ( accursedFollower, AccursedFollower (..),) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype AccursedFollower = AccursedFollower EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

accursedFollower :: EnemyCard AccursedFollower
accursedFollower =
  enemyWith
    AccursedFollower
    Cards.accursedFollower
    (2, Static 2, 2)
    (1, 1)
    (spawnAtL ?~ SpawnAt (FarthestLocationFromYou Anywhere))

instance HasAbilities AccursedFollower where
  getAbilities (AccursedFollower a) = extend a [restrictedAbility a 1 HasRemainingCurseTokens $ forced $ PhaseEnds #when #enemy]

instance RunMessage AccursedFollower where
  runMessage msg e@(AccursedFollower attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addCurseTokens 1
      pure e
    _ -> AccursedFollower <$> liftRunMessage msg attrs
