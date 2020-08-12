{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.RiseOfTheGhouls where

import Arkham.Json
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Trait
import ClassyPrelude hiding (sequence)
import Lens.Micro

newtype RiseOfTheGhouls = RiseOfTheGhouls Attrs
  deriving newtype (Show, ToJSON, FromJSON)

riseOfTheGhouls :: RiseOfTheGhouls
riseOfTheGhouls = RiseOfTheGhouls
  $ baseAttrs "01106" "Rise of the Ghouls" "Agenda 2a" (Static 7)

instance (AgendaRunner env) => RunMessage env RiseOfTheGhouls where
  runMessage msg a@(RiseOfTheGhouls attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 2a" -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
      unshiftMessage (Ask leadInvestigatorId $ ChooseOne [AdvanceAgenda aid])
      pure $ RiseOfTheGhouls $ attrs & sequence .~ "Agenda 2b" & flipped .~ True
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 2b" ->
      a <$ unshiftMessage
        (Run
          [ ShuffleEncounterDiscardBackIn
          , DiscardEncounterUntilFirst (AgendaSource aid) (EnemyType, Ghoul)
          ]
        )
    RequestedEncounterCard (AgendaSource aid) mcard | aid == agendaId ->
      case mcard of
        Nothing -> a <$ unshiftMessage (NextAgenda aid "01107")
        Just card -> do
          leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
          a <$ unshiftMessages
            [ InvestigatorDrewEncounterCard leadInvestigatorId card
            , NextAgenda aid "01107"
            ]
    _ -> RiseOfTheGhouls <$> runMessage msg attrs
