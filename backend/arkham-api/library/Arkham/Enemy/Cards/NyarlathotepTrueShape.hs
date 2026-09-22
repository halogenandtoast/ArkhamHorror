module Arkham.Enemy.Cards.NyarlathotepTrueShape (
  nyarlathotepTrueShape,
  NyarlathotepTrueShape (..),
)
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype NyarlathotepTrueShape = NyarlathotepTrueShape EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

nyarlathotepTrueShape :: EnemyCard NyarlathotepTrueShape
nyarlathotepTrueShape = enemy NyarlathotepTrueShape Cards.nyarlathotepTrueShape

instance RunMessage NyarlathotepTrueShape where
  runMessage msg e@(NyarlathotepTrueShape attrs) = runQueueT $ case msg of
    -- He gets -1 health per clue the investigators hold, so gaining a clue can drop his
    -- health to or below the damage already on him. Defeat is only rechecked when damage
    -- is assigned, never when health falls.
    After (GainClues {}) -> do
      checkDefeated GameSource attrs
      pure e
    _ -> NyarlathotepTrueShape <$> liftRunMessage msg attrs
