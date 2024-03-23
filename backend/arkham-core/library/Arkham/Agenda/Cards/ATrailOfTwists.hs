module Arkham.Agenda.Cards.ATrailOfTwists (
  ATrailOfTwists (..),
  aTrailOfTwists,
) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype ATrailOfTwists = ATrailOfTwists AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTrailOfTwists :: AgendaCard ATrailOfTwists
aTrailOfTwists = agenda (2, A) ATrailOfTwists Cards.aTrailOfTwists (Static 9)

instance RunMessage ATrailOfTwists where
  runMessage msg a@(ATrailOfTwists attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> ATrailOfTwists <$> runMessage msg attrs
