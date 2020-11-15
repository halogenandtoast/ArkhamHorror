{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.RiseOfTheGhouls where

import Arkham.Import hiding (sequence)

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Trait

newtype RiseOfTheGhouls = RiseOfTheGhouls Attrs
  deriving newtype (Show, ToJSON, FromJSON)

riseOfTheGhouls :: RiseOfTheGhouls
riseOfTheGhouls = RiseOfTheGhouls
  $ baseAttrs "01106" 2 "Rise of the Ghouls" "Agenda 2a" (Static 7)

instance HasActions env RiseOfTheGhouls where
  getActions i window (RiseOfTheGhouls x) = getActions i window x

instance AgendaRunner env => RunMessage env RiseOfTheGhouls where
  runMessage msg a@(RiseOfTheGhouls attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 2a" -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage (chooseOne leadInvestigatorId [AdvanceAgenda aid])
      pure $ RiseOfTheGhouls $ attrs & sequence .~ "Agenda 2b" & flipped .~ True
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 2b" ->
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
