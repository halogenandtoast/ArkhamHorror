module Arkham.Enemy.Cards.FelineHybrid (felineHybrid) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (immuneToPlayerEffects)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher

newtype FelineHybrid = FelineHybrid EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

instance HasModifiersFor FelineHybrid where
  getModifiersFor (FelineHybrid a) = immuneToPlayerEffects a

felineHybrid :: EnemyCard FelineHybrid
felineHybrid =
  enemy FelineHybrid Cards.felineHybrid (3, Static 2, 3) (1, 1)
    & setOnlyPrey (investigatorIs Investigators.miguelDeLaCruz)

instance RunMessage FelineHybrid where
  runMessage msg (FelineHybrid attrs) = FelineHybrid <$> runMessage msg attrs
