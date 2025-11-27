module Arkham.Enemy.Cards.VoidChimeraFellbeak (voidChimeraFellbeak) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)

newtype VoidChimeraFellbeak = VoidChimeraFellbeak EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

voidChimeraFellbeak :: EnemyCard VoidChimeraFellbeak
voidChimeraFellbeak = enemy VoidChimeraFellbeak Cards.voidChimeraFellbeak (4, Static 2, 2) (3, 0)

instance HasModifiersFor VoidChimeraFellbeak where
  getModifiersFor (VoidChimeraFellbeak a) = do
    n <- perPlayer 1
    modifySelf a [HealthModifier n]
