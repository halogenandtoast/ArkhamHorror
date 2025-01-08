module Arkham.Enemy.Cards.AngryMob (angryMob, AngryMob (..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)

newtype AngryMob = AngryMob EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

angryMob :: EnemyCard AngryMob
angryMob = enemy AngryMob Cards.angryMob (3, Static 4, 3) (1, 1)

instance HasModifiersFor AngryMob where
  getModifiersFor (AngryMob a) = do
    n <- perPlayer 4
    modifySelf a [HealthModifier n, CannotMakeAttacksOfOpportunity]

instance RunMessage AngryMob where
  runMessage msg (AngryMob attrs) =
    AngryMob <$> runMessage msg attrs
