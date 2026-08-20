module Arkham.Enemy.Cards.ReturnToNightOfTheZealot.ReturnToTheGathering.GhoulFromTheDepths (ghoulFromTheDepths) where

import Arkham.Enemy.CardDefs.ReturnToNightOfTheZealot.ReturnToTheGathering qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype GhoulFromTheDepths = GhoulFromTheDepths EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ghoulFromTheDepths :: EnemyCard GhoulFromTheDepths
ghoulFromTheDepths =
  enemy GhoulFromTheDepths Cards.ghoulFromTheDepths
    & setSpawnAt "Bathroom"

instance RunMessage GhoulFromTheDepths where
  runMessage msg (GhoulFromTheDepths attrs) = GhoulFromTheDepths <$> runMessage msg attrs
