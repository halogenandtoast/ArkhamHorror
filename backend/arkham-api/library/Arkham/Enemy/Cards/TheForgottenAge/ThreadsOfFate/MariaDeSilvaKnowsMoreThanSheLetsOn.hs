module Arkham.Enemy.Cards.TheForgottenAge.ThreadsOfFate.MariaDeSilvaKnowsMoreThanSheLetsOn (mariaDeSilvaKnowsMoreThanSheLetsOn) where

import Arkham.Enemy.CardDefs.TheForgottenAge.ThreadsOfFate qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MariaDeSilvaKnowsMoreThanSheLetsOn = MariaDeSilvaKnowsMoreThanSheLetsOn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mariaDeSilvaKnowsMoreThanSheLetsOn
  :: EnemyCard MariaDeSilvaKnowsMoreThanSheLetsOn
mariaDeSilvaKnowsMoreThanSheLetsOn =
  enemy
    MariaDeSilvaKnowsMoreThanSheLetsOn
    Cards.mariaDeSilvaKnowsMoreThanSheLetsOn

instance RunMessage MariaDeSilvaKnowsMoreThanSheLetsOn where
  runMessage msg (MariaDeSilvaKnowsMoreThanSheLetsOn attrs) =
    MariaDeSilvaKnowsMoreThanSheLetsOn <$> runMessage msg attrs
