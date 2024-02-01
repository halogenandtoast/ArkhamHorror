module Arkham.Agenda.Cards.LockedInside (
  LockedInside (..),
  lockedInside,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Scenario.Deck

newtype LockedInside = LockedInside AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

lockedInside :: AgendaCard LockedInside
lockedInside = agenda (1, A) LockedInside Cards.lockedInside (Static 2)

instance RunMessage LockedInside where
  runMessage msg a@(LockedInside attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      a
        <$ pushAll
          [ ShuffleScenarioDeckIntoEncounterDeck LunaticsDeck
          , ShuffleEncounterDiscardBackIn
          , DrawRandomFromScenarioDeck
              leadInvestigatorId
              MonstersDeck
              (toTarget attrs)
              1
          , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
          ]
    DrewFromScenarioDeck _ _ target cards
      | isTarget attrs target ->
          a <$ push (PlaceUnderneath ActDeckTarget cards)
    _ -> LockedInside <$> runMessage msg attrs
