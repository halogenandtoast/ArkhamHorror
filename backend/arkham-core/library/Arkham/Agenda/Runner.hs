{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Agenda.Runner (module Arkham.Agenda.Runner, module X) where

import Arkham.Prelude

import Arkham.ActId
import Arkham.Agenda.AdvancementReason
import Arkham.Agenda.Attrs as X
import Arkham.Agenda.Sequence as X
import Arkham.Act.Attrs (ActAttrs)
import Arkham.Card
import Arkham.Classes
import Arkham.Direction
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message
import Arkham.Name
import Arkham.Query
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

instance RunMessage AgendaAttrs
  where
  runMessage msg a@AgendaAttrs {..} = case msg of
    PlaceUnderneath target cards | isTarget a target ->
      pure $ a & cardsUnderneathL %~ (<> cards)
    PlaceDoom (AgendaTarget aid) n | aid == agendaId -> pure $ a & doomL +~ n
    Discard (TreacheryTarget tid) -> pure $ a & treacheriesL %~ deleteSet tid
    AttachTreachery tid (AgendaTarget aid) | aid == agendaId ->
      pure $ a & treacheriesL %~ insertSet tid
    AdvanceAgenda aid | aid == agendaId && agendaSide agendaSequence == A -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push $ chooseOne leadInvestigatorId [AdvanceAgenda agendaId]
      pure
        $ a
        & (sequenceL .~ Agenda (unAgendaStep $ agendaStep agendaSequence) B)
        & (flippedL .~ True)
    AdvanceAgendaIfThresholdSatisfied -> do
      perPlayerDoomThreshold <- getPlayerCountValue (a ^. doomThresholdL)
      totalDoom <- getDoomCount
      when
        (totalDoom >= perPlayerDoomThreshold)
        do
          whenMsg <- checkWindows
            [ Window
                Timing.When
                (Window.AgendaWouldAdvance DoomThreshold $ toId a)
            ]
          afterMsg <- checkWindows
            [ Window
                Timing.After
                (Window.AgendaWouldAdvance DoomThreshold $ toId a)
            ]
          pushAll [whenMsg, afterMsg, Do AdvanceAgendaIfThresholdSatisfied]
      pure a
    Do AdvanceAgendaIfThresholdSatisfied -> do
      -- This status can change due to the above windows so we much check again
      perPlayerDoomThreshold <- getPlayerCountValue (a ^. doomThresholdL)
      totalDoom <- getDoomCount
      when (totalDoom >= perPlayerDoomThreshold) $ do
        leadInvestigatorId <- getLeadInvestigatorId
        pushAll
          [ CheckWindow
            [leadInvestigatorId]
            [Window Timing.When (Window.AgendaAdvance agendaId)]
          , AdvanceAgenda agendaId
          , RemoveAllDoom
          ]
      pure a
    RemoveAllDoom -> do
      pure $ a & doomL .~ 0
    RevertAgenda aid | aid == agendaId && onSide B a ->
      pure
        $ a
        & (sequenceL .~ Agenda (unAgendaStep $ agendaStep agendaSequence) A)
    _ -> pure a
