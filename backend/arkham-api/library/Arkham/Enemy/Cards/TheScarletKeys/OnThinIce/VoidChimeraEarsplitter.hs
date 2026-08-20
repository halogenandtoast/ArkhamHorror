module Arkham.Enemy.Cards.TheScarletKeys.OnThinIce.VoidChimeraEarsplitter (voidChimeraEarsplitter) where

import Arkham.Enemy.CardDefs.TheScarletKeys.OnThinIce qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)

newtype VoidChimeraEarsplitter = VoidChimeraEarsplitter EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

voidChimeraEarsplitter :: EnemyCard VoidChimeraEarsplitter
voidChimeraEarsplitter = enemy VoidChimeraEarsplitter Cards.voidChimeraEarsplitter

instance HasModifiersFor VoidChimeraEarsplitter where
  getModifiersFor (VoidChimeraEarsplitter a) = do
    n <- perPlayer 1
    modifySelf a [HealthModifier n]
