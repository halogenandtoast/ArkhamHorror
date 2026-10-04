module Arkham.Homebrew.ConsternationOnTheConstellation.Enemies.DagonWarrior (dagonWarrior) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Enemies qualified as Cards

newtype DagonWarrior = DagonWarrior EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Deep Ones. Forced after the first Hazard treachery is revealed each round: Dagon
Warrior attacks each investigator at its location.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
dagonWarrior :: EnemyCard DagonWarrior
dagonWarrior = enemy DagonWarrior Cards.dagonWarrior

instance RunMessage DagonWarrior where
  runMessage msg (DagonWarrior attrs) = runQueueT $ DagonWarrior <$> liftRunMessage msg attrs
