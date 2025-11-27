module Arkham.Enemy.Cards.VoidChimeraEarsplitter (voidChimeraEarsplitter) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)

newtype VoidChimeraEarsplitter = VoidChimeraEarsplitter EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

voidChimeraEarsplitter :: EnemyCard VoidChimeraEarsplitter
voidChimeraEarsplitter = enemy VoidChimeraEarsplitter Cards.voidChimeraEarsplitter (3, Static 3, 4) (0, 3)

instance HasModifiersFor VoidChimeraEarsplitter where
  getModifiersFor (VoidChimeraEarsplitter a) = do
    n <- perPlayer 1
    modifySelf a [HealthModifier n]
