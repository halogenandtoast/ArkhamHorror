{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Act.Runner (
  module Arkham.Act.Runner,
  module X,
) where

import Arkham.Prelude

import Arkham.Act.Helpers as X
import Arkham.Act.Sequence as X
import Arkham.Act.Types as X
import Arkham.Calculation as X
import Arkham.Cost as X
import Arkham.GameValue as X
import Arkham.Helpers.Act as X
import Arkham.Helpers.Message as X hiding (
  Discarded,
  EnemyDamage,
  EnemyDamaged,
  EnemyDefeated,
  EnemyEvaded,
  InvestigatorEliminated,
  PaidCost,
  RevealChaosToken,
 )
import Arkham.Helpers.SkillTest as X
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Target as X

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Matcher hiding (FastPlayerWindow)
import Arkham.Tarot
import Arkham.Timing qualified as Timing
import Arkham.Window
import Arkham.Window qualified as Window

advanceActDeck :: ActAttrs -> Message
advanceActDeck attrs = AdvanceActDeck (actDeckId attrs) (toSource attrs)

advanceActSideA
  :: HasGame m => ActAttrs -> AdvancementMethod -> m [Message]
advanceActSideA attrs advanceMode = do
  (iid, lead) <- getLeadInvestigatorPlayer
  pure
    [ CheckWindow [iid] [mkWhen (ActAdvance $ toId attrs)]
    , chooseOne
        lead
        [TargetLabel (ActTarget $ toId attrs) [AdvanceAct (toId attrs) (toSource attrs) advanceMode]]
    ]

instance RunMessage Act where
  runMessage msg (Act a) = Act <$> runMessage msg a

onFrontSide :: ActAttrs -> Bool
onFrontSide = (`elem` [A, C, E]) . actSide . actSequence

backSide :: ActAttrs -> ActSide
backSide attrs = case actSide $ actSequence attrs of
  A -> B
  C -> D
  E -> F
  _ -> error "backSide: not on front side"

instance RunMessage ActAttrs where
  runMessage msg a@ActAttrs {..} = case msg of
    AdvanceAct aid _ advanceMode | aid == actId && onFrontSide a -> do
      pushAll =<< advanceActSideA a advanceMode
      pure $ a & (sequenceL .~ Sequence (unActStep $ actStep actSequence) (backSide a))
    AttachTreachery tid (ActTarget aid) | aid == actId -> do
      pure $ a & treacheriesL %~ insertSet tid
    Discard _ _ (ActTarget aid) | aid == toId a -> do
      pushAll
        [toDiscard GameSource (TreacheryTarget tid) | tid <- setToList actTreacheries]
      pure a
    Discard _ _ (TreacheryTarget tid) -> pure $ a & treacheriesL %~ deleteSet tid
    InvestigatorResigned _ -> do
      investigatorIds <- select UneliminatedInvestigator
      whenMsg <-
        checkWindows
          [mkWindow Timing.When AllUndefeatedInvestigatorsResigned]
      afterMsg <-
        checkWindows
          [mkWindow Timing.When AllUndefeatedInvestigatorsResigned]
      when
        (null investigatorIds)
        (pushAll [whenMsg, afterMsg, AllInvestigatorsResigned])
      pure a
    UseCardAbility iid source 999 _ _ | isSource a source -> do
      -- This is assumed to be advancement via spending clues
      push $ AdvanceAct (toId a) (InvestigatorSource iid) AdvancedWithClues
      pure a
    PlaceClues _ (ActTarget aid) n | aid == actId -> do
      let totalClues = n + actClues
      pure $ a {actClues = totalClues}
    PlaceBreaches (isTarget a -> True) n -> do
      let total = maybe 0 (+ n) actBreaches
      pure $ a & breachesL ?~ total
    RemoveBreaches (isTarget a -> True) n -> do
      wouldDoEach
        n
        (RemoveBreaches (toTarget a) 1)
        (Window.WouldRemoveBreaches (toTarget a))
        (Window.WouldRemoveBreach (toTarget a))
        (Window.RemovedBreaches (toTarget a))
        (Window.RemovedBreach (toTarget a))
      pure a
    DoBatch _ (RemoveBreaches (isTarget a -> True) n) -> do
      pure $ a & breachesL %~ fmap (max 0 . subtract n)
    UseCardAbility
      iid
      source@(TarotSource (TarotCard Upright WheelOfFortuneX))
      1
      (Window.revealedChaosTokens -> [token])
      _ -> do
        pushAll
          [ ChaosTokenCanceled iid source token
          , chaosTokenEffect source token $ ChaosTokenFaceModifier [Zero]
          ]
        pure $ a {actUsedWheelOfFortuneX = True}
    _ -> pure a
