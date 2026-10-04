module Arkham.Homebrew.ConsternationOnTheConstellation.Enemies.LutherMarsh (lutherMarsh) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Enemies qualified as Cards

newtype LutherMarsh = LutherMarsh EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Spawned at the Bridge by agenda 2b with the Tablet of Dagon attached. He cannot
be defeated by damage; after he attacks you, a failed [combat] (3) test pushes
you to a connecting location. Victory 2.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
lutherMarsh :: EnemyCard LutherMarsh
lutherMarsh = enemy LutherMarsh Cards.lutherMarsh

instance RunMessage LutherMarsh where
  runMessage msg (LutherMarsh attrs) = runQueueT $ LutherMarsh <$> liftRunMessage msg attrs
