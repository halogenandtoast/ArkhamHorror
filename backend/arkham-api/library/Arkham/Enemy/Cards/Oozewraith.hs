module Arkham.Enemy.Cards.Oozewraith (oozewraith) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Oozified))

newtype Oozewraith = Oozewraith EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

oozewraith :: EnemyCard Oozewraith
oozewraith =
  enemy Oozewraith Cards.oozewraith
    & setSpawnAt (FarthestLocationFromYou $ LocationWithTrait Oozified)
