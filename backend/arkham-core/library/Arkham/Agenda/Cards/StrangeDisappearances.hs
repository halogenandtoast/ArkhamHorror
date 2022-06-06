module Arkham.Agenda.Cards.StrangeDisappearances
  ( StrangeDisappearances(..)
  , strangeDisappearances
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Attrs
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message
import Arkham.Query
import Arkham.Scenario.Deck
import Arkham.Target

newtype StrangeDisappearances = StrangeDisappearances AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeDisappearances :: AgendaCard StrangeDisappearances
strangeDisappearances =
  agenda (1, A) StrangeDisappearances Cards.strangeDisappearances (Static 6)

instance AgendaRunner env => RunMessage StrangeDisappearances where
  runMessage msg a@(StrangeDisappearances attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      scenarioDeckCount <- unScenarioDeckCount <$> getCount PotentialSacrifices
      if scenarioDeckCount >= 3
        then a <$ pushAll
          [ DrawRandomFromScenarioDeck
            leadInvestigatorId
            PotentialSacrifices
            (toTarget attrs)
            1
          , AdvanceAgendaDeck agendaDeckId (toSource attrs)
          ]
        else a <$ push (AdvanceAgendaDeck agendaDeckId (toSource attrs))
    DrewFromScenarioDeck _ PotentialSacrifices target cards
      | isTarget attrs target -> a
      <$ push (PlaceUnderneath AgendaDeckTarget cards)
    _ -> StrangeDisappearances <$> runMessage msg attrs
