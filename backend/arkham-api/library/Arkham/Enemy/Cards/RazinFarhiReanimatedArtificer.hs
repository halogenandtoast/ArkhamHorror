module Arkham.Enemy.Cards.RazinFarhiReanimatedArtificer (razinFarhiReanimatedArtificer) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype RazinFarhiReanimatedArtificer = RazinFarhiReanimatedArtificer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

razinFarhiReanimatedArtificer :: EnemyCard RazinFarhiReanimatedArtificer
razinFarhiReanimatedArtificer = enemy RazinFarhiReanimatedArtificer Cards.razinFarhiReanimatedArtificer (4, PerPlayer 4, 3) (1, 1)

instance RunMessage RazinFarhiReanimatedArtificer where
  runMessage msg (RazinFarhiReanimatedArtificer attrs) = runQueueT $ case msg of
    _ -> RazinFarhiReanimatedArtificer <$> liftRunMessage msg attrs
