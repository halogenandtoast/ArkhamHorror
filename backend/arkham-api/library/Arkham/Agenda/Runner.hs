{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Agenda.Runner (
  module X,
  advanceAgendaDeck,
) where

import Arkham.Prelude

import Arkham.Agenda.Helpers as X
import Arkham.Agenda.Sequence as X
import Arkham.Agenda.Types as X
import Arkham.Calculation as X
import Arkham.Helpers.Effect as X
import Arkham.Helpers.Message as X hiding (EnemyDefeated, InvestigatorEliminated)
import Arkham.Helpers.SkillTest as X
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Agenda.AdvancementReason
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Tarot
import Arkham.Window (mkAfter, mkWhen)
import Arkham.Window qualified as Window

advanceAgendaDeck :: AgendaAttrs -> Message
advanceAgendaDeck attrs = AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)

instance RunMessage AgendaAttrs where
  runMessage msg a@AgendaAttrs {..} = case msg of
    PlaceUnderneath target cards | isTarget a target -> do
      pure $ a & cardsUnderneathL %~ (<> cards)
    PlaceDoom source (isTarget a -> True) n -> do
      when (a.doom == 0) do
        pushM $ checkAfter $ Window.PlacedDoomCounterOnTargetWithNoDoom source (toTarget a) n

      wouldDo msg (Window.WouldPlaceDoom source (toTarget a) n) (Window.PlacedDoom source (toTarget a) n)
      pure a
    DoBatch _ (PlaceDoom _ (isTarget a -> True) n) -> do
      pure $ a & doomL +~ n
    RemoveDoom _ (AgendaTarget aid) n | aid == agendaId -> do
      pure $ a & doomL %~ max 0 . subtract n
    AdvanceAgendaBy aid advanceMethod | aid == agendaId && agendaSide agendaSequence == A -> do
      lead <- getLeadPlayer
      push $ chooseOne lead [targetLabel agendaId [AdvanceAgendaBy agendaId advanceMethod]]
      pure
        $ a
        & (sequenceL .~ Sequence (unAgendaStep $ agendaStep agendaSequence) B)
        & (flippedL .~ True)
    AdvanceAgendaBy aid advanceMethod | aid == agendaId && agendaSide agendaSequence == C -> do
      lead <- getLeadPlayer
      push $ chooseOne lead [targetLabel agendaId [AdvanceAgendaBy agendaId advanceMethod]]
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
            otherDoomSubtracts = OtherDoomSubtracts `elem` modifiers'
          -- handle multiple agendas, this might need to be specific to the
          -- scenario, but for now given there is only once scenario and the rules
          -- are likely to be the same in the future
          otherAgendaDoom <-
            getSum
              <$> selectAgg Sum AgendaDoom (NotAgenda $ AgendaWithId $ toId a)
          doomCount <- if otherDoomSubtracts then getSubtractDoomCount else getDoomCount
          let
            totalDoom =
              if otherDoomSubtracts
                then a.doom - (doomCount - a.doom)
                else subtract otherAgendaDoom doomCount
          when (totalDoom >= modifiedPerPlayerDoomThreshold) do
            whenMsg <- checkWindows [mkWhen (Window.AgendaWouldAdvance DoomThreshold $ toId a)]
            afterMsg <- checkWindows [mkAfter (Window.AgendaWouldAdvance DoomThreshold $ toId a)]
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
            otherDoomSubtracts = OtherDoomSubtracts `elem` modifiers'
          otherAgendaDoom <-
            getSum
              <$> selectAgg Sum AgendaDoom (NotAgenda $ AgendaWithId $ toId a)
          doomCount <- if otherDoomSubtracts then getSubtractDoomCount else getDoomCount
          let
            totalDoom =
              if otherDoomSubtracts
                then a.doom - (doomCount - a.doom)
                else subtract otherAgendaDoom doomCount
          when (totalDoom >= modifiedPerPlayerDoomThreshold) $ do
            whenWindow <- checkWhen $ Window.AgendaAdvance agendaId
            afterWindow <- checkAfter $ Window.AgendaAdvance agendaId
            pushAll
              [ whenWindow
              , RemoveAllDoomFromPlay agendaRemoveDoomMatchers
              , AdvanceAgenda agendaId
              , afterWindow
              ]
          pure a
    RemoveAllDoom _ (isTarget a -> True) -> do
      pure $ a & doomL .~ 0
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
        enabled <- chaosTokenEffect source token $ ChaosTokenFaceModifier [MinusFive]
        pushAll
          [ ChaosTokenCanceled iid source token
          , enabled
          ]
        pure $ a {agendaUsedWheelOfFortuneX = True}
    _ -> pure a
