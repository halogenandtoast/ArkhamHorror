module Arkham.Types.Agenda.Cards.RiseOfTheGhouls where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Trait

newtype RiseOfTheGhouls = RiseOfTheGhouls AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riseOfTheGhouls :: AgendaCard RiseOfTheGhouls
riseOfTheGhouls =
  agenda (2, A) RiseOfTheGhouls Cards.riseOfTheGhouls (Static 7)

instance HasModifiersFor env RiseOfTheGhouls
instance HasActions RiseOfTheGhouls

instance AgendaRunner env => RunMessage env RiseOfTheGhouls where
  runMessage msg a@(RiseOfTheGhouls attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B ->
      a <$ push
        (Run
          [ ShuffleEncounterDiscardBackIn
          , DiscardEncounterUntilFirst
            (AgendaSource aid)
            (CardWithType EnemyType <> CardWithTrait Ghoul)
          ]
        )
    RequestedEncounterCard (AgendaSource aid) mcard | aid == agendaId ->
      case mcard of
        Nothing -> a <$ push (NextAgenda aid "01107")
        Just card -> do
          leadInvestigatorId <- getLeadInvestigatorId
          a <$ pushAll
            [ InvestigatorDrewEncounterCard leadInvestigatorId card
            , NextAgenda aid "01107"
            ]
    _ -> RiseOfTheGhouls <$> runMessage msg attrs
