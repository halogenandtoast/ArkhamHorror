module Arkham.Types.Agenda.Cards.StrangeDisappearances
  ( StrangeDisappearances(..)
  , strangeDisappearances
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Scenario.Deck
import Arkham.Types.Target

newtype StrangeDisappearances = StrangeDisappearances AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeDisappearances :: AgendaCard StrangeDisappearances
strangeDisappearances =
  agenda (1, A) StrangeDisappearances Cards.strangeDisappearances (Static 6)

instance AgendaRunner env => RunMessage env StrangeDisappearances where
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
          , NextAgenda agendaId "02197"
          ]
        else a <$ push (NextAgenda agendaId "02197")
    DrewFromScenarioDeck _ PotentialSacrifices target cards
      | isTarget attrs target -> a
      <$ push (PlaceUnderneath AgendaDeckTarget cards)
    _ -> StrangeDisappearances <$> runMessage msg attrs
