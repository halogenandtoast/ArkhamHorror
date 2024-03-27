module Arkham.Enemy.Cards.AccursedFollower (
  accursedFollower,
  AccursedFollower (..),
)
where

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

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
  runMessage msg e@(AccursedFollower attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ AddChaosToken CurseToken
      pure e
    _ -> AccursedFollower <$> runMessage msg attrs
