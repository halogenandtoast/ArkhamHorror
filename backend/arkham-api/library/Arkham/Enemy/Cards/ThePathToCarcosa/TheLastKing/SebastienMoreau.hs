module Arkham.Enemy.Cards.ThePathToCarcosa.TheLastKing.SebastienMoreau (sebastienMoreau) where

import Arkham.Enemy.CardDefs.ThePathToCarcosa.TheLastKing qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers

newtype SebastienMoreau = SebastienMoreau EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sebastienMoreau :: EnemyCard SebastienMoreau
sebastienMoreau = enemy SebastienMoreau Cards.sebastienMoreau

instance HasModifiersFor SebastienMoreau where
  getModifiersFor (SebastienMoreau a) = modifySelf a [AttacksCannotBeCancelled]

instance RunMessage SebastienMoreau where
  runMessage msg (SebastienMoreau attrs) = SebastienMoreau <$> runMessage msg attrs
