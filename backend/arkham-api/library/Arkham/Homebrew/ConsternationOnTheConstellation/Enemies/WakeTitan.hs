module Arkham.Homebrew.ConsternationOnTheConstellation.Enemies.WakeTitan (wakeTitan) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Enemies qualified as Cards

newtype WakeTitan = WakeTitan EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Deep Ones. Forced: exhaust the location it spawns at, and ready that location
again once it is defeated. Victory 1.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
wakeTitan :: EnemyCard WakeTitan
wakeTitan = enemy WakeTitan Cards.wakeTitan

instance RunMessage WakeTitan where
  runMessage msg (WakeTitan attrs) = runQueueT $ WakeTitan <$> liftRunMessage msg attrs
