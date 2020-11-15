{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.TheArkhamWoods where

import Arkham.Import hiding (sequence)

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Trait

newtype TheArkhamWoods = TheArkhamWoods Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theArkhamWoods :: TheArkhamWoods
theArkhamWoods =
  TheArkhamWoods $ baseAttrs "01143" 1 "The Arkham Woods" "Agenda 1a" (Static 4)

instance HasActions env TheArkhamWoods where
  getActions i window (TheArkhamWoods x) = getActions i window x

instance AgendaRunner env => RunMessage env TheArkhamWoods where
  runMessage msg a@(TheArkhamWoods attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 1a" -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage (chooseOne leadInvestigatorId [AdvanceAgenda aid])
      pure $ TheArkhamWoods $ attrs & sequence .~ "Agenda 1b" & flipped .~ True
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 1b" ->
      a <$ unshiftMessage
        (Run
          [ ShuffleEncounterDiscardBackIn
          , DiscardEncounterUntilFirst
            (AgendaSource aid)
            (EncounterCardMatchByType (EnemyType, Just Monster))
          ]
        )
    RequestedEncounterCard (AgendaSource aid) mcard | aid == agendaId ->
      case mcard of
        Nothing -> a <$ unshiftMessage (NextAgenda aid "01144")
        Just card -> do
          a <$ unshiftMessages
            [ SpawnEnemyAt (EncounterCard card) "01149"
            , PlaceDoom (CardIdTarget $ getCardId card) 1
            , NextAgenda aid "01144"
            ]
    _ -> TheArkhamWoods <$> runMessage msg attrs
