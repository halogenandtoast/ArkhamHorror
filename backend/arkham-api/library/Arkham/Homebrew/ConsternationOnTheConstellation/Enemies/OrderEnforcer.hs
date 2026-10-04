module Arkham.Homebrew.ConsternationOnTheConstellation.Enemies.OrderEnforcer (orderEnforcer) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype OrderEnforcer = OrderEnforcer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | One copy is spawned at Cargo Room during setup and is the whole of act 1. When it
attacks you, discarding a non-story Item asset reduces the damage by 1. Agenda 1a
gives every copy +1 health per investigator plus aloof and Elite.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
orderEnforcer :: EnemyCard OrderEnforcer
orderEnforcer = enemy OrderEnforcer Cards.orderEnforcer & setPrey MostClues

instance RunMessage OrderEnforcer where
  runMessage msg (OrderEnforcer attrs) = runQueueT $ OrderEnforcer <$> liftRunMessage msg attrs
