module Arkham.Types.Agenda.Cards.RiseOfTheGhouls where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Trait

newtype RiseOfTheGhouls = RiseOfTheGhouls AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riseOfTheGhouls :: RiseOfTheGhouls
riseOfTheGhouls = RiseOfTheGhouls
  $ baseAttrs "01106" "Rise of the Ghouls" (Agenda 2 A) (Static 7)

instance HasModifiersFor env RiseOfTheGhouls where
  getModifiersFor = noModifiersFor

instance HasActions env RiseOfTheGhouls where
  getActions i window (RiseOfTheGhouls x) = getActions i window x

instance AgendaRunner env => RunMessage env RiseOfTheGhouls where
  runMessage msg a@(RiseOfTheGhouls attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B ->
      a <$ unshiftMessage
        (Run
          [ ShuffleEncounterDiscardBackIn
          , DiscardEncounterUntilFirst
            (AgendaSource aid)
            (EncounterCardMatchByType (EnemyType, Just Ghoul))
          ]
        )
    RequestedEncounterCard (AgendaSource aid) mcard | aid == agendaId ->
      case mcard of
        Nothing -> a <$ unshiftMessage (NextAgenda aid "01107")
        Just card -> do
          leadInvestigatorId <- getLeadInvestigatorId
          a <$ unshiftMessages
            [ InvestigatorDrewEncounterCard leadInvestigatorId card
            , NextAgenda aid "01107"
            ]
    _ -> RiseOfTheGhouls <$> runMessage msg attrs
