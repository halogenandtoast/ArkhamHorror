module Arkham.Homebrew.ConsternationOnTheConstellation.Enemies.CultistOfTheDeep (cultistOfTheDeep) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Enemies qualified as Cards

newtype CultistOfTheDeep = CultistOfTheDeep EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Spawns at the nearest location holding a Crate of Goods, or any empty Deck
location when none is in play. While it is at a location, encounter card effects
treat that location as exhausted.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
cultistOfTheDeep :: EnemyCard CultistOfTheDeep
cultistOfTheDeep = enemy CultistOfTheDeep Cards.cultistOfTheDeep

instance RunMessage CultistOfTheDeep where
  runMessage msg (CultistOfTheDeep attrs) = runQueueT $ CultistOfTheDeep <$> liftRunMessage msg attrs
