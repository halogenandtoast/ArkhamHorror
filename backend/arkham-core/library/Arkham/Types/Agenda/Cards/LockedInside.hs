module Arkham.Types.Agenda.Cards.LockedInside
  ( LockedInside
  , lockedInside
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Scenario.Deck
import Arkham.Types.Target

newtype LockedInside = LockedInside AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockedInside :: AgendaCard LockedInside
lockedInside = agenda (1, A) LockedInside Cards.lockedInside (Static 2)

instance AgendaRunner env => RunMessage env LockedInside where
  runMessage msg a@(LockedInside attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ pushAll
        [ ShuffleScenarioDeckIntoEncounterDeck LunaticsDeck
        , ShuffleEncounterDiscardBackIn
        , DrawRandomFromScenarioDeck
          leadInvestigatorId
          MonstersDeck
          (toTarget attrs)
          1
        , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        ]
    DrewFromScenarioDeck _ _ target cards | isTarget attrs target ->
      a <$ push (PlaceUnderneath ActDeckTarget cards)
    _ -> LockedInside <$> runMessage msg attrs
