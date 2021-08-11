module Arkham.Types.Agenda.Cards.StrangeDisappearances
  ( StrangeDisappearances(..)
  , strangeDisappearances
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Query

newtype StrangeDisappearances = StrangeDisappearances AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeDisappearances :: AgendaCard StrangeDisappearances
strangeDisappearances =
  agenda (1, A) StrangeDisappearances Cards.strangeDisappearances (Static 6)

instance HasModifiersFor env StrangeDisappearances
instance HasActions StrangeDisappearances

instance AgendaRunner env => RunMessage env StrangeDisappearances where
  runMessage msg a@(StrangeDisappearances attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      scenarioDeckCount <- unScenarioDeckCount <$> getCount ()
      if scenarioDeckCount >= 3
        then a <$ pushAll
          [ UseScenarioSpecificAbility leadInvestigatorId Nothing 1
          , NextAgenda agendaId "02197"
          ]
        else a <$ push (NextAgenda agendaId "02197")
    _ -> StrangeDisappearances <$> runMessage msg attrs
