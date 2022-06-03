module Arkham.Agenda.Cards.TheFestivitiesBegin
  ( TheFestivitiesBegin
  , theFestivitiesBegin
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Agenda.Attrs
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheFestivitiesBegin = TheFestivitiesBegin AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFestivitiesBegin :: AgendaCard TheFestivitiesBegin
theFestivitiesBegin =
  agenda (1, A) TheFestivitiesBegin Cards.theFestivitiesBegin (Static 8)

instance AgendaRunner env => RunMessage TheFestivitiesBegin where
  runMessage msg a@(TheFestivitiesBegin attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B -> do
      leadInvestigatorId <- getLeadInvestigatorId
      balefulReveler <- genEncounterCard Enemies.balefulReveler
      a <$ pushAll
        [ InvestigatorDrewEncounterCard leadInvestigatorId balefulReveler
        , AdvanceAgendaDeck agendaDeckId (toSource attrs)
        ]
    _ -> TheFestivitiesBegin <$> runMessage msg attrs
