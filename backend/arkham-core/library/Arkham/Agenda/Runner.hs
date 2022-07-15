{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Agenda.Runner
  ( module X
  ) where

import Arkham.Prelude

import Arkham.Agenda.AdvancementReason
import Arkham.Agenda.Attrs as X
import Arkham.Agenda.Sequence as X
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Matcher hiding ( PlaceUnderneath )
import Arkham.Message
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

instance RunMessage AgendaAttrs
  where
  runMessage msg a@AgendaAttrs {..} = case msg of
    PlaceUnderneath target cards | isTarget a target ->
      pure $ a & cardsUnderneathL %~ (<> cards)
    PlaceDoom (AgendaTarget aid) n | aid == agendaId -> do
      windows' <- windows [Window.PlacedDoom (toTarget a) n]
      pushAll windows'
      pure $ a & doomL +~ n
    RemoveDoom (AgendaTarget aid) n | aid == agendaId ->
      pure $ a & doomL %~ max 0 . subtract n
    Discard (TreacheryTarget tid) -> pure $ a & treacheriesL %~ deleteSet tid
    Discard (AgendaTarget aid) | aid == toId a -> do
      pushAll
        [ Discard (TreacheryTarget tid) | tid <- setToList agendaTreacheries ]
      pure a
    AttachTreachery tid (AgendaTarget aid) | aid == agendaId ->
      pure $ a & treacheriesL %~ insertSet tid
    AdvanceAgenda aid | aid == agendaId && agendaSide agendaSequence == A -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push $ chooseOne
        leadInvestigatorId
        [TargetLabel (AgendaTarget agendaId) [AdvanceAgenda agendaId]]
      pure
        $ a
        & (sequenceL .~ Agenda (unAgendaStep $ agendaStep agendaSequence) B)
        & (flippedL .~ True)
    AdvanceAgenda aid | aid == agendaId && agendaSide agendaSequence == C -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push $ chooseOne
        leadInvestigatorId
        [TargetLabel (AgendaTarget agendaId) [AdvanceAgenda agendaId]]
      pure
        $ a
        & (sequenceL .~ Agenda (unAgendaStep $ agendaStep agendaSequence) D)
        & (flippedL .~ True)
    AdvanceAgendaIfThresholdSatisfied -> do
      perPlayerDoomThreshold <- getPlayerCountValue (a ^. doomThresholdL)
      -- handle multiple agendas, this might need to be specific to the
      -- scenario, but for now given there is only once scenario and the rules
      -- are likely to be the same in the future
      otherAgendaDoom <- getSum
        <$> selectAgg Sum AgendaDoom (NotAgenda $ AgendaWithId $ toId a)
      totalDoom <- subtract otherAgendaDoom <$> getDoomCount
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
      otherAgendaDoom <- getSum
        <$> selectAgg Sum AgendaDoom (NotAgenda $ AgendaWithId $ toId a)
      totalDoom <- subtract otherAgendaDoom <$> getDoomCount
      when (totalDoom >= perPlayerDoomThreshold) $ do
        leadInvestigatorId <- getLeadInvestigatorId
        pushAll
          [ CheckWindow
            [leadInvestigatorId]
            [Window Timing.When (Window.AgendaAdvance agendaId)]
          , RemoveAllDoom (toSource a)
          , AdvanceAgenda agendaId
          ]
      pure a
    RemoveAllDoom source -> do
      if toSource a == source then pure $ a & doomL .~ 0 else pure a
    RevertAgenda aid | aid == agendaId && onSide B a ->
      pure
        $ a
        & (sequenceL .~ Agenda (unAgendaStep $ agendaStep agendaSequence) A)
    _ -> pure a
