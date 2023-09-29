{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Agenda.Runner (
  module X,
  advanceAgendaDeck,
) where

import Arkham.Prelude

import Arkham.Agenda.Helpers as X
import Arkham.Agenda.Sequence as X
import Arkham.Agenda.Types as X
import Arkham.Helpers.Message as X hiding (EnemyDefeated, InvestigatorEliminated)
import Arkham.Helpers.SkillTest as X
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Agenda.AdvancementReason
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Tarot
import Arkham.Timing qualified as Timing
import Arkham.Window (Window, mkWindow, windowType)
import Arkham.Window qualified as Window

advanceAgendaDeck :: AgendaAttrs -> Message
advanceAgendaDeck attrs = AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)

getChaosToken :: [Window] -> ChaosToken
getChaosToken = \case
  [] -> error "No chaos token drawn"
  ((windowType -> Window.RevealChaosToken _ token) : _) -> token
  (_ : rest) -> getChaosToken rest

instance RunMessage AgendaAttrs where
  runMessage msg a@AgendaAttrs {..} = case msg of
    PlaceUnderneath target cards | isTarget a target -> do
      pure $ a & cardsUnderneathL %~ (<> cards)
    PlaceDoom source (isTarget a -> True) n -> do
      wouldDo msg (Window.WouldPlaceDoom source (toTarget a) n) (Window.PlacedDoom source (toTarget a) n)
      pure a
    DoBatch _ (PlaceDoom _ (isTarget a -> True) n) -> do
      pure $ a & doomL +~ n
    RemoveDoom _ (AgendaTarget aid) n | aid == agendaId -> do
      pure $ a & doomL %~ max 0 . subtract n
    Discard _ (TreacheryTarget tid) -> pure $ a & treacheriesL %~ deleteSet tid
    Discard _ (AgendaTarget aid) | aid == toId a -> do
      pushAll
        [Discard GameSource (TreacheryTarget tid) | tid <- setToList agendaTreacheries]
      pure a
    AttachTreachery tid (AgendaTarget aid)
      | aid == agendaId ->
          pure $ a & treacheriesL %~ insertSet tid
    AdvanceAgenda aid | aid == agendaId && agendaSide agendaSequence == A -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push
        $ chooseOne
          leadInvestigatorId
          [targetLabel agendaId [AdvanceAgenda agendaId]]
      pure
        $ a
        & (sequenceL .~ Sequence (unAgendaStep $ agendaStep agendaSequence) B)
        & (flippedL .~ True)
    AdvanceAgenda aid | aid == agendaId && agendaSide agendaSequence == C -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push
        $ chooseOne
          leadInvestigatorId
          [targetLabel agendaId [AdvanceAgenda agendaId]]
      pure
        $ a
        & (sequenceL .~ Sequence (unAgendaStep $ agendaStep agendaSequence) D)
        & (flippedL .~ True)
    AdvanceAgendaIfThresholdSatisfied -> do
      cannotBeAdvanced <- hasModifier a CannotBeAdvancedByDoomThreshold
      unless cannotBeAdvanced $ do
        for_ (a ^. doomThresholdL) $ \threshold -> do
          perPlayerDoomThreshold <- getPlayerCountValue threshold
          modifiers' <- getModifiers (toTarget a)
          let
            modifyDoomThreshold acc = \case
              DoomThresholdModifier n -> max 0 (acc + n)
              _ -> acc
            modifiedPerPlayerDoomThreshold =
              foldl' modifyDoomThreshold perPlayerDoomThreshold modifiers'
          -- handle multiple agendas, this might need to be specific to the
          -- scenario, but for now given there is only once scenario and the rules
          -- are likely to be the same in the future
          otherAgendaDoom <-
            getSum
              <$> selectAgg Sum AgendaDoom (NotAgenda $ AgendaWithId $ toId a)
          totalDoom <- subtract otherAgendaDoom <$> getDoomCount
          when
            (totalDoom >= modifiedPerPlayerDoomThreshold)
            do
              whenMsg <-
                checkWindows
                  [ mkWindow
                      Timing.When
                      (Window.AgendaWouldAdvance DoomThreshold $ toId a)
                  ]
              afterMsg <-
                checkWindows
                  [ mkWindow
                      Timing.After
                      (Window.AgendaWouldAdvance DoomThreshold $ toId a)
                  ]
              pushAll [whenMsg, afterMsg, Do AdvanceAgendaIfThresholdSatisfied]
      pure a
    Do AdvanceAgendaIfThresholdSatisfied -> do
      case a ^. doomThresholdL of
        Nothing -> error "can not advance without threshold"
        Just threshold -> do
          -- This status can change due to the above windows so we much check again
          perPlayerDoomThreshold <- getPlayerCountValue threshold
          modifiers' <- getModifiers (toTarget a)
          let
            modifyDoomThreshold acc = \case
              DoomThresholdModifier n -> max 0 (acc + n)
              _ -> acc
            modifiedPerPlayerDoomThreshold =
              foldl' modifyDoomThreshold perPlayerDoomThreshold modifiers'
          otherAgendaDoom <-
            getSum
              <$> selectAgg Sum AgendaDoom (NotAgenda $ AgendaWithId $ toId a)
          totalDoom <- subtract otherAgendaDoom <$> getDoomCount
          when (totalDoom >= modifiedPerPlayerDoomThreshold) $ do
            leadInvestigatorId <- getLeadInvestigatorId
            pushAll
              [ CheckWindow
                  [leadInvestigatorId]
                  [mkWindow Timing.When (Window.AgendaAdvance agendaId)]
              , RemoveAllDoomFromPlay agendaRemoveDoomMatchers
              , AdvanceAgenda agendaId
              ]
          pure a
    RevertAgenda aid | aid == agendaId && agendaFlipped -> do
      pure
        $ a
        & (sequenceL .~ flipSequence agendaSequence)
        & flippedL
        .~ False
    UseCardAbility
      iid
      source@(TarotSource (TarotCard Reversed WheelOfFortuneX))
      1
      (getChaosToken -> token)
      _ -> do
        pushAll
          [ ChaosTokenCanceled iid source token
          , chaosTokenEffect source token $ ChaosTokenFaceModifier [MinusFive]
          ]
        pure $ a {agendaUsedWheelOfFortuneX = True}
    _ -> pure a
