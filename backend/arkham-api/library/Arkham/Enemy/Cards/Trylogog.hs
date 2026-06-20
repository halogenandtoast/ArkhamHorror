module Arkham.Enemy.Cards.Trylogog (trylogog, trylogogWarOfTheOuterGods) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype Trylogog = Trylogog EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trylogog :: EnemyCard Trylogog
trylogog =
  enemyWith Trylogog Cards.trylogog
    $ spawnAtL
    ?~ SpawnAt (locationIs Locations.theBurningPit)

trylogogWarOfTheOuterGods :: EnemyCard Trylogog
trylogogWarOfTheOuterGods =
  enemyWith Trylogog Cards.trylogogWarOfTheOuterGods
    $ spawnAtL
    ?~ SpawnAt (locationIs Locations.theBurningPit)

instance HasModifiersFor Trylogog where
  getModifiersFor (Trylogog a) = modifySelf a [AttackDealsEitherDamageOrHorror]

instance RunMessage Trylogog where
  runMessage msg (Trylogog attrs) = Trylogog <$> runMessage msg attrs
