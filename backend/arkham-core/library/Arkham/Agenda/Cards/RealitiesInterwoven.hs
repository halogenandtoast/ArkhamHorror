module Arkham.Agenda.Cards.RealitiesInterwoven (
  RealitiesInterwoven (..),
  realitiesInterwoven,
) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype RealitiesInterwoven = RealitiesInterwoven AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realitiesInterwoven :: AgendaCard RealitiesInterwoven
realitiesInterwoven = agenda (3, A) RealitiesInterwoven Cards.realitiesInterwoven (Static 11)

instance RunMessage RealitiesInterwoven where
  runMessage msg a@(RealitiesInterwoven attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> RealitiesInterwoven <$> runMessage msg attrs
