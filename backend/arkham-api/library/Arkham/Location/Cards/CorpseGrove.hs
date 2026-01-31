module Arkham.Location.Cards.CorpseGrove (corpseGrove) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CorpseGrove = CorpseGrove LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corpseGrove :: LocationCard CorpseGrove
corpseGrove = locationWith CorpseGrove Cards.corpseGrove 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities CorpseGrove where
  getAbilities (CorpseGrove a) =
    extendRevealed a []

instance RunMessage CorpseGrove where
  runMessage msg (CorpseGrove attrs) = runQueueT $ case msg of
    _ -> CorpseGrove <$> liftRunMessage msg attrs
