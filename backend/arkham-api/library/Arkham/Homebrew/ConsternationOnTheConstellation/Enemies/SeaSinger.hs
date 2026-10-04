module Arkham.Homebrew.ConsternationOnTheConstellation.Enemies.SeaSinger (seaSinger) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Enemies qualified as Cards

newtype SeaSinger = SeaSinger EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Aloof, and seals a (+1) or [elder_sign] token. While it holds a sealed token that
token counts as doom for checking the agenda's doom threshold.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
seaSinger :: EnemyCard SeaSinger
seaSinger = enemy SeaSinger Cards.seaSinger

instance RunMessage SeaSinger where
  runMessage msg (SeaSinger attrs) = runQueueT $ SeaSinger <$> liftRunMessage msg attrs
