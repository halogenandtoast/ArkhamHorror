module Arkham.Enemy.Cards.NightOfTheZealot.Ghouls.RavenousGhoul (ravenousGhoul) where

import Arkham.Enemy.CardDefs.NightOfTheZealot.Ghouls qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype RavenousGhoul = RavenousGhoul EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ravenousGhoul :: EnemyCard RavenousGhoul
ravenousGhoul =
  enemyWith
    RavenousGhoul
    Cards.ravenousGhoul
    (preyL .~ Prey LowestRemainingHealth)

instance RunMessage RavenousGhoul where
  runMessage msg (RavenousGhoul attrs) = RavenousGhoul <$> runMessage msg attrs
