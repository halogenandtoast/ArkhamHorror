module Arkham.Enemy.Cards.WarOfTheOuterGods.Trylogog (trylogog, trylogogWarOfTheOuterGods) where

import Arkham.Enemy.CardDefs.WarOfTheOuterGods qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.CardDefs.WarOfTheOuterGods qualified as Locations
import Arkham.Matcher

newtype Trylogog = Trylogog EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

trylogog :: EnemyCard Trylogog
trylogog =
  enemy Trylogog Cards.trylogog
    & setSpawnAt (locationIs Locations.theBurningPit)

trylogogWarOfTheOuterGods :: EnemyCard Trylogog
trylogogWarOfTheOuterGods =
  enemy Trylogog Cards.trylogogWarOfTheOuterGods
    & setSpawnAt (locationIs Locations.theBurningPit)

instance HasModifiersFor Trylogog where
  getModifiersFor (Trylogog a) = modifySelf a [AttackDealsEitherDamageOrHorror]

instance RunMessage Trylogog where
  runMessage msg (Trylogog attrs) = Trylogog <$> runMessage msg attrs
