module Arkham.Enemy.Cards.VoltaicEel (voltaicEel) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen)
import Arkham.Matcher

newtype VoltaicEel = VoltaicEel EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

voltaicEel :: EnemyCard VoltaicEel
voltaicEel =
  enemyWith
    VoltaicEel
    Cards.voltaicEel
    (preyL .~ Prey MostClues)

instance HasModifiersFor VoltaicEel where
  getModifiersFor (VoltaicEel a) = do
    isMoving <- a.id <=~> MovingEnemy
    modifySelectWhen a isMoving Anywhere [ConnectedToWhen FullyFloodedLocation FullyFloodedLocation]

instance RunMessage VoltaicEel where
  runMessage msg (VoltaicEel attrs) = runQueueT $ VoltaicEel <$> liftRunMessage msg attrs
