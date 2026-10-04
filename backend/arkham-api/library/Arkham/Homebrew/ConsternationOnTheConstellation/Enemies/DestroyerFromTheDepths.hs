module Arkham.Homebrew.ConsternationOnTheConstellation.Enemies.DestroyerFromTheDepths (destroyerFromTheDepths) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Enemies qualified as Cards

newtype DestroyerFromTheDepths = DestroyerFromTheDepths EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Deep Ones. Forced when the enemy phase begins: discard the top 3 cards of the
encounter deck and draw each copy of Taking on Water among them.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
destroyerFromTheDepths :: EnemyCard DestroyerFromTheDepths
destroyerFromTheDepths = enemy DestroyerFromTheDepths Cards.destroyerFromTheDepths

instance RunMessage DestroyerFromTheDepths where
  runMessage msg (DestroyerFromTheDepths attrs) = runQueueT $ DestroyerFromTheDepths <$> liftRunMessage msg attrs
