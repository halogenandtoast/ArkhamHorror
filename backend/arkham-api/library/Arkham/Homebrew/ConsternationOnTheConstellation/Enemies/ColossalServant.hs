module Arkham.Homebrew.ConsternationOnTheConstellation.Enemies.ColossalServant (colossalServant) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Enemies qualified as Cards

newtype ColossalServant = ColossalServant EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Spawned at Open Water by act 2b -- the other face of Luther Marsh. Forced at the
beginning of the investigator phase: reveal a chaos token, and on a non-symbol
move it to the location with the most investigators. With nobody at its location
it returns to Open Water. Victory 2.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
colossalServant :: EnemyCard ColossalServant
colossalServant = enemy ColossalServant Cards.colossalServant

instance RunMessage ColossalServant where
  runMessage msg (ColossalServant attrs) = runQueueT $ ColossalServant <$> liftRunMessage msg attrs
