module Arkham.Agenda.Cards.TheLoversVI (
  TheLoversVI (..),
  theLoversVI,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheLoversVI = TheLoversVI AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLoversVI :: AgendaCard TheLoversVI
theLoversVI = agenda (1, A) TheLoversVI Cards.theLoversVI (Static 8)

instance RunMessage TheLoversVI where
  runMessage msg a@(TheLoversVI attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        takenByTheWatcher <- length <$> getRecordSet WasTakenByTheWatcher
        if takenByTheWatcher > 0
          then pushAll [advanceAgendaDeck attrs]
          else pushAll [advanceAgendaDeck attrs]
        pure a
      _ -> TheLoversVI <$> runMessage msg attrs
