module Arkham.Agenda.Cards.TheFestivitiesBegin (theFestivitiesBegin) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Prelude

newtype TheFestivitiesBegin = TheFestivitiesBegin AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFestivitiesBegin :: AgendaCard TheFestivitiesBegin
theFestivitiesBegin = agenda (1, A) TheFestivitiesBegin Cards.theFestivitiesBegin (Static 8)

instance RunMessage TheFestivitiesBegin where
  runMessage msg a@(TheFestivitiesBegin attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      leadInvestigatorId <- getLead
      balefulReveler <- genEncounterCard Enemies.balefulReveler
      pushAll
        [ InvestigatorDrewEncounterCard leadInvestigatorId balefulReveler
        , AdvanceAgendaDeck agendaDeckId (toSource attrs)
        ]
      pure a
    _ -> TheFestivitiesBegin <$> runMessage msg attrs
