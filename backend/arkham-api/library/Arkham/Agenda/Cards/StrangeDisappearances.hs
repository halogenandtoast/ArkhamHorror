module Arkham.Agenda.Cards.StrangeDisappearances (StrangeDisappearances (..), strangeDisappearances) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Choose
import Arkham.Prelude
import Arkham.Scenario.Deck

newtype StrangeDisappearances = StrangeDisappearances AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeDisappearances :: AgendaCard StrangeDisappearances
strangeDisappearances = agenda (1, A) StrangeDisappearances Cards.strangeDisappearances (Static 6)

instance RunMessage StrangeDisappearances where
  runMessage msg a@(StrangeDisappearances attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      lead <- getLeadInvestigatorId
      scenarioDeckCount <- length <$> getScenarioDeck PotentialSacrifices
      if scenarioDeckCount >= 3
        then
          pushAll
            [ randomlyChooseFrom attrs lead PotentialSacrifices 1
            , AdvanceAgendaDeck agendaDeckId (toSource attrs)
            ]
        else push (AdvanceAgendaDeck agendaDeckId (toSource attrs))
      pure a
    ChoseCards _ chose | isTarget attrs chose.target -> do
      push $ PlaceUnderneath AgendaDeckTarget chose.cards
      pure a
    _ -> StrangeDisappearances <$> runMessage msg attrs
