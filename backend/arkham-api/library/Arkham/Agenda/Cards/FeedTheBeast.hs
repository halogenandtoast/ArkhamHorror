module Arkham.Agenda.Cards.FeedTheBeast (feedTheBeast) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype FeedTheBeast = FeedTheBeast AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feedTheBeast :: AgendaCard FeedTheBeast
feedTheBeast = agenda (3, A) FeedTheBeast Cards.feedTheBeast (Static 7)

instance RunMessage FeedTheBeast where
  runMessage msg a@(FeedTheBeast attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator resign
      pure a
    _ -> FeedTheBeast <$> liftRunMessage msg attrs
