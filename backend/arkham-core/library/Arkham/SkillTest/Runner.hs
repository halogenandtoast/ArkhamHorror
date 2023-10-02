{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.SkillTest.Runner (
  module X,
  totalModifiedSkillValue,
) where

import Arkham.Prelude

import Arkham.SkillTest as X

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes hiding (matches)
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Card
import Arkham.Helpers.Message
import Arkham.Matcher hiding (IgnoreChaosToken, RevealChaosToken)
import Arkham.RequestedChaosTokenStrategy
import Arkham.SkillTestResult
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..), mkWindow)
import Arkham.Window qualified as Window
import Control.Lens (each)
import Data.Map.Strict qualified as Map

totalModifiedSkillValue :: HasGame m => SkillTest -> m Int
totalModifiedSkillValue s = do
  results <- calculateSkillTestResultsData s
  chaosTokenValues <-
    sum
      <$> for
        (skillTestResolvedChaosTokens s)
        (getModifiedChaosTokenValue s)

  let totaledChaosTokenValues = chaosTokenValues + skillTestValueModifier s
  pure
    $ max
      0
      (skillTestResultsSkillValue results + totaledChaosTokenValues + skillTestResultsIconValue results)

calculateSkillTestResultsData :: HasGame m => SkillTest -> m SkillTestResultsData
calculateSkillTestResultsData s = do
  modifiers' <- getModifiers SkillTestTarget
  modifiedSkillTestDifficulty <- getModifiedSkillTestDifficulty s
  let cancelSkills = CancelSkills `elem` modifiers'
  iconCount <- if cancelSkills then pure 0 else skillIconCount s
  subtractIconCount <- if cancelSkills then pure 0 else subtractSkillIconCount s
  currentSkillValue <- getCurrentSkillValue s
  chaosTokenValues <-
    sum
      <$> for
        (skillTestRevealedChaosTokens s <> skillTestResolvedChaosTokens s)
        (getModifiedChaosTokenValue s)
  let
    addResultModifier n (SkillTestResultValueModifier m) = n + m
    addResultModifier n _ = n
    resultValueModifiers = foldl' addResultModifier 0 modifiers'
    totaledChaosTokenValues = chaosTokenValues + (skillTestValueModifier s)
    modifiedSkillValue' =
      max 0 (currentSkillValue + totaledChaosTokenValues + iconCount - subtractIconCount)
    op = if FailTies `elem` modifiers' then (>) else (>=)
    isSuccess = modifiedSkillValue' `op` modifiedSkillTestDifficulty
  pure
    $ SkillTestResultsData
      currentSkillValue
      (iconCount - subtractIconCount)
      totaledChaosTokenValues
      modifiedSkillTestDifficulty
      (resultValueModifiers <$ guard (resultValueModifiers /= 0))
      isSuccess

autoFailSkillTestResultsData :: HasGame m => SkillTest -> m SkillTestResultsData
autoFailSkillTestResultsData s = do
  modifiedSkillTestDifficulty <- getModifiedSkillTestDifficulty s
  chaosTokenValues <-
    sum
      <$> for
        (skillTestRevealedChaosTokens s <> skillTestResolvedChaosTokens s)
        (getModifiedChaosTokenValue s)
  let
    totaledChaosTokenValues = chaosTokenValues + (skillTestValueModifier s)
  pure $ SkillTestResultsData 0 0 totaledChaosTokenValues modifiedSkillTestDifficulty Nothing False

subtractSkillIconCount :: HasGame m => SkillTest -> m Int
subtractSkillIconCount SkillTest {..} =
  count matches <$> concatMapM iconsForCard (concat $ toList skillTestCommittedCards)
 where
  matches WildMinusIcon = True
  matches WildIcon = False
  matches (SkillIcon _) = False

-- per the FAQ the double negative modifier ceases to be active
-- when Sure Gamble is used so we overwrite both Negative and DoubleNegative
getModifiedChaosTokenValue :: HasGame m => SkillTest -> ChaosToken -> m Int
getModifiedChaosTokenValue s t = do
  tokenModifiers' <- getModifiers (ChaosTokenTarget t)
  modifiedChaosTokenFaces' <- getModifiedChaosTokenFaces [t]
  getSum
    <$> foldMapM
      ( \chaosTokenFace -> do
          baseChaosTokenValue <- getChaosTokenValue (skillTestInvestigator s) chaosTokenFace ()
          let
            updatedChaosTokenValue =
              chaosTokenValue $ foldr applyModifier baseChaosTokenValue tokenModifiers'
          pure . Sum $ fromMaybe 0 updatedChaosTokenValue
      )
      modifiedChaosTokenFaces'
 where
  applyModifier IgnoreChaosToken (ChaosTokenValue token _) = ChaosTokenValue token NoModifier
  applyModifier (ChangeChaosTokenModifier modifier') (ChaosTokenValue token _) =
    ChaosTokenValue token modifier'
  applyModifier NegativeToPositive (ChaosTokenValue token (NegativeModifier n)) =
    ChaosTokenValue token (PositiveModifier n)
  applyModifier NegativeToPositive (ChaosTokenValue token (DoubleNegativeModifier n)) =
    ChaosTokenValue token (PositiveModifier n)
  applyModifier DoubleNegativeModifiersOnChaosTokens (ChaosTokenValue token (NegativeModifier n)) =
    ChaosTokenValue token (DoubleNegativeModifier n)
  applyModifier (ChaosTokenValueModifier m) (ChaosTokenValue token (PositiveModifier n)) =
    ChaosTokenValue token (PositiveModifier (max 0 (n + m)))
  applyModifier (ChaosTokenValueModifier m) (ChaosTokenValue token (NegativeModifier n)) =
    ChaosTokenValue token (NegativeModifier (max 0 (n - m)))
  applyModifier _ currentChaosTokenValue = currentChaosTokenValue

instance RunMessage SkillTest where
  runMessage msg s@SkillTest {..} = case msg of
    BeginSkillTestAfterFast -> do
      windowMsg <- checkWindows [mkWindow #when Window.FastPlayerWindow]
      pushAll [windowMsg, BeforeSkillTest s, EndSkillTestWindow]
      pure s
    ReplaceSkillTestSkill (FromSkillType fsType) (ToSkillType tsType) -> do
      let
        stType = case skillTestType of
          ResourceSkillTest -> ResourceSkillTest
          SkillSkillTest currentType -> if currentType == fsType then SkillSkillTest tsType else SkillSkillTest currentType
          AndSkillTest types -> AndSkillTest $ map (\t -> if t == fsType then tsType else t) types
        stBaseValue = case skillTestBaseValue of
          SkillBaseValue currentType -> SkillBaseValue $ if currentType == fsType then tsType else currentType
          AndSkillBaseValue xs -> AndSkillBaseValue $ map (\t -> if t == fsType then tsType else t) xs
          HalfResourcesOf x -> HalfResourcesOf x
          StaticBaseValue x -> StaticBaseValue x

      pure
        $ s
          { skillTestType = stType
          , skillTestBaseValue = stBaseValue
          }
    SetSkillTestTarget target -> do
      pure $ s {skillTestTarget = target}
    Discard _ target | target == skillTestTarget -> do
      pushAll
        [ SkillTestEnds skillTestInvestigator skillTestSource
        , Do (SkillTestEnds skillTestInvestigator skillTestSource)
        ]
      pure s
    RemoveFromGame target | target == skillTestTarget -> do
      pushAll
        [ SkillTestEnds skillTestInvestigator skillTestSource
        , Do (SkillTestEnds skillTestInvestigator skillTestSource)
        ]
      pure s
    TriggerSkillTest iid -> do
      modifiers' <- getModifiers iid
      if DoNotDrawChaosTokensForSkillChecks `elem` modifiers'
        then do
          let
            tokensTreatedAsRevealed = flip mapMaybe modifiers' $ \case
              TreatRevealedChaosTokenAs t -> Just t
              _ -> Nothing
          if null tokensTreatedAsRevealed
            then push (RunSkillTest iid)
            else do
              pushAll [RevealSkillTestChaosTokens iid, RunSkillTest iid]
              for_ tokensTreatedAsRevealed $ \chaosTokenFace -> do
                t <- getRandom
                pushAll
                  $ resolve (RevealChaosToken (toSource s) iid (ChaosToken t chaosTokenFace))
        else
          if SkillTestAutomaticallySucceeds `elem` modifiers'
            then pushAll [PassSkillTest, UnsetActiveCard]
            else do
              let
                applyRevealStategyModifier _ (ChangeRevealStrategy n) = n
                applyRevealStategyModifier n _ = n
                revealStrategy =
                  foldl' applyRevealStategyModifier (Reveal 1) modifiers'
              pushAll
                [ RequestChaosTokens (toSource s) (Just iid) revealStrategy SetAside
                , RunSkillTest iid
                ]
      pure s
    DrawAnotherChaosToken iid -> do
      withQueue_ $ filter $ \case
        Will FailedSkillTest {} -> False
        Will PassedSkillTest {} -> False
        CheckWindow _ [Window Timing.When (Window.WouldFailSkillTest _) _] ->
          False
        CheckWindow _ [Window Timing.When (Window.WouldPassSkillTest _) _] ->
          False
        RunWindow _ [Window Timing.When (Window.WouldPassSkillTest _) _] -> False
        RunWindow _ [Window Timing.When (Window.WouldFailSkillTest _) _] -> False
        Ask skillTestInvestigator' (ChooseOne [SkillTestApplyResultsButton])
          | skillTestInvestigator == skillTestInvestigator' -> False
        _ -> True
      pushAll
        [ RequestChaosTokens (toSource s) (Just iid) (Reveal 1) SetAside
        , RunSkillTest iid
        ]
      pure $ s & (resolvedChaosTokensL %~ (<> skillTestRevealedChaosTokens))
    RequestedChaosTokens SkillTestSource (Just iid) chaosTokenFaces ->
      do
        skillTestModifiers' <- getModifiers SkillTestTarget
        windowMsg <- checkWindows [mkWindow Timing.When Window.FastPlayerWindow]
        push
          $ if RevealChaosTokensBeforeCommittingCards `elem` skillTestModifiers'
            then
              CommitToSkillTest
                s
                ( Label "Done Comitting" $ [CheckAllAdditionalCommitCosts, windowMsg, RevealSkillTestChaosTokens iid]
                )
            else RevealSkillTestChaosTokens iid
        for_ chaosTokenFaces $ \chaosTokenFace -> do
          let
            revealMsg = RevealChaosToken SkillTestSource iid chaosTokenFace
          pushAll
            [ When revealMsg
            , CheckWindow [iid] [mkWindow Timing.AtIf (Window.RevealChaosToken iid chaosTokenFace)]
            , revealMsg
            , After revealMsg
            ]
        pure $ s & (setAsideChaosTokensL %~ (chaosTokenFaces <>))
    RevealChaosToken SkillTestSource {} iid token -> do
      push
        (CheckWindow [iid] [mkWindow Timing.After (Window.RevealChaosToken iid token)])
      pure $ s & revealedChaosTokensL %~ (token :)
    RevealSkillTestChaosTokens iid -> do
      revealedChaosTokenFaces <- flip
        concatMapM
        (skillTestRevealedChaosTokens \\ skillTestResolvedChaosTokens)
        \token -> do
          faces <- getModifiedChaosTokenFaces [token]
          pure [(token, face) | face <- faces]
      pushAll
        [ ResolveChaosToken drawnChaosToken chaosTokenFace iid
        | (drawnChaosToken, chaosTokenFace) <- revealedChaosTokenFaces
        ]
      pure
        $ s
        & ( subscribersL
              %~ (nub . (<> [ChaosTokenTarget token' | token' <- skillTestRevealedChaosTokens]))
          )
    PassSkillTest -> do
      currentSkillValue <- getCurrentSkillValue s
      iconCount <- skillIconCount s
      let
        modifiedSkillValue' =
          max 0 (currentSkillValue + skillTestValueModifier + iconCount)
      pushAll
        [ chooseOne skillTestInvestigator [SkillTestApplyResultsButton]
        , SkillTestEnds skillTestInvestigator skillTestSource
        , Do (SkillTestEnds skillTestInvestigator skillTestSource)
        ]
      pure $ s & resultL .~ SucceededBy Automatic modifiedSkillValue'
    FailSkillTest -> do
      resultsData <- autoFailSkillTestResultsData s
      difficulty <- getModifiedSkillTestDifficulty s
      pushAll
        $ SkillTestResults resultsData
        : [ Will
            ( FailedSkillTest
                skillTestInvestigator
                skillTestAction
                skillTestSource
                target
                skillTestType
                difficulty
            )
          | target <- skillTestSubscribers
          ]
          <> [ Will
                ( FailedSkillTest
                    skillTestInvestigator
                    skillTestAction
                    skillTestSource
                    (SkillTestInitiatorTarget skillTestTarget)
                    skillTestType
                    difficulty
                )
             , chooseOne skillTestInvestigator [SkillTestApplyResultsButton]
             , SkillTestEnds skillTestInvestigator skillTestSource
             , Do (SkillTestEnds skillTestInvestigator skillTestSource)
             ]
      pure $ s & resultL .~ FailedBy Automatic difficulty
    StartSkillTest _ -> do
      windowMsg <- checkWindows [mkWindow Timing.When Window.FastPlayerWindow]
      pushAll [CheckAllAdditionalCommitCosts, windowMsg, TriggerSkillTest skillTestInvestigator]
      pure s
    CheckAllAdditionalCommitCosts -> do
      pushAll $ Map.foldMapWithKey (\i cs -> [CheckAdditionalCommitCosts i cs]) skillTestCommittedCards
      pure s
    CheckAdditionalCommitCosts iid cards -> do
      modifiers' <- getModifiers iid
      let
        msgs = map (CommitCard iid) cards
        additionalCosts =
          mapMaybe
            ( \case
                CommitCost c -> Just c
                _ -> Nothing
            )
            modifiers'
      if null additionalCosts
        then pushAll msgs
        else do
          canPay <-
            getCanAffordCost
              iid
              (toSource s)
              Nothing
              [mkWindow Timing.When Window.NonFast]
              (mconcat additionalCosts)
          iid' <- getActiveInvestigatorId
          when canPay
            $ pushAll
            $ [SetActiveInvestigator iid | iid /= iid']
            <> [PayForAbility (abilityEffect s $ mconcat additionalCosts) []]
            <> [SetActiveInvestigator iid' | iid /= iid']
            <> msgs
      pure s
    InvestigatorCommittedSkill _ skillId ->
      pure $ s & subscribersL %~ (nub . (SkillTarget skillId :))
    PutCardOnBottomOfDeck _ _ card -> do
      pure $ s & committedCardsL %~ map (filter (/= card))
    PutCardOnTopOfDeck _ _ card -> do
      pure $ s & committedCardsL %~ map (filter (/= card))
    PutCardIntoPlay _ card _ _ -> do
      pure $ s & committedCardsL %~ map (filter (/= card))
    CardEnteredPlay _ card -> do
      pure $ s & committedCardsL %~ map (filter (/= card))
    SkillTestCommitCard iid card -> do
      pure $ s & committedCardsL %~ insertWith (<>) iid [card]
    CommitCard iid card | card `notElem` findWithDefault [] iid skillTestCommittedCards -> do
      pure $ s & committedCardsL %~ insertWith (<>) iid [card]
    SkillTestUncommitCard _ card ->
      pure $ s & committedCardsL %~ map (filter (/= card))
    ReturnSkillTestRevealedChaosTokens -> do
      -- Rex's Curse timing keeps effects on stack so we do
      -- not want to remove them as subscribers from the stack
      push $ ResetChaosTokens (toSource s)
      pure
        $ s
        & (setAsideChaosTokensL .~ mempty)
        & (revealedChaosTokensL .~ mempty)
        & (resolvedChaosTokensL .~ mempty)
        & (valueModifierL .~ 0)
    Do (SkillTestEnds _ _) -> do
      -- Skill Cards are in the environment and will be discarded normally
      -- However, all other cards need to be discarded here.
      let
        discards =
          concatMap
            ( \case
                (iid, cards) -> flip mapMaybe cards $ \case
                  PlayerCard pc -> (iid, pc) <$ guard (cdCardType (toCardDef pc) /= SkillType)
                  EncounterCard _ -> Nothing
                  VengeanceCard _ -> Nothing
            )
            (s ^. committedCardsL . to mapToList)

      skillTestEndsWindows <- windows [Window.SkillTestEnded s]
      pushAll
        $ ResetChaosTokens (toSource s)
        : map (uncurry AddToDiscard) discards
          <> skillTestEndsWindows
          <> [ AfterSkillTestEnds
                skillTestSource
                skillTestTarget
                skillTestResult
             ]
      pure s
    ReturnToHand _ (CardIdTarget cardId) -> do
      pure $ s & committedCardsL . each %~ filter ((/= cardId) . toCardId)
    ReturnToHand _ (CardTarget card) -> do
      pure $ s & committedCardsL . each %~ filter (/= card)
    SkillTestResults {} -> do
      modifiers' <- getModifiers (toTarget s)
      -- We may be recalculating so we want to remove all windows an buttons to apply
      removeAllMessagesMatching $ \case
        Will (PassedSkillTest {}) -> True
        Will (FailedSkillTest {}) -> True
        Ask _ (ChooseOne [SkillTestApplyResultsButton]) -> True
        _ -> False

      push (chooseOne skillTestInvestigator [SkillTestApplyResultsButton])
      let
        modifiedSkillTestResult =
          foldl' modifySkillTestResult skillTestResult modifiers'
        modifySkillTestResult r (SkillTestResultValueModifier n) = case r of
          Unrun -> Unrun
          SucceededBy b m -> SucceededBy b (max 0 (m + n))
          FailedBy b m -> FailedBy b (max 0 (m + n))
        modifySkillTestResult r _ = r
      case modifiedSkillTestResult of
        SucceededBy _ n ->
          pushAll
            ( [ Will
                ( PassedSkillTest
                    skillTestInvestigator
                    skillTestAction
                    skillTestSource
                    target
                    skillTestType
                    n
                )
              | target <- skillTestSubscribers
              ]
                <> [ Will
                      ( PassedSkillTest
                          skillTestInvestigator
                          skillTestAction
                          skillTestSource
                          (SkillTestInitiatorTarget skillTestTarget)
                          skillTestType
                          n
                      )
                   ]
            )
        FailedBy _ n ->
          pushAll
            ( [ Will
                ( FailedSkillTest
                    skillTestInvestigator
                    skillTestAction
                    skillTestSource
                    target
                    skillTestType
                    n
                )
              | target <- skillTestSubscribers
              ]
                <> [ Will
                      ( FailedSkillTest
                          skillTestInvestigator
                          skillTestAction
                          skillTestSource
                          (SkillTestInitiatorTarget skillTestTarget)
                          skillTestType
                          n
                      )
                   ]
            )
        Unrun -> pure ()
      pure s
    SkillTestApplyResultsAfter -> do
      -- ST.7 -- apply results
      pushAll
        [ SkillTestEnds skillTestInvestigator skillTestSource
        , Do (SkillTestEnds skillTestInvestigator skillTestSource) -- -> ST.8 -- Skill test ends
        ]
      modifiers' <- getModifiers (toTarget s)
      let
        modifiedSkillTestResult =
          foldl' modifySkillTestResult skillTestResult modifiers'
        modifySkillTestResult r (SkillTestResultValueModifier n) = case r of
          Unrun -> Unrun
          SucceededBy b m -> SucceededBy b (max 0 (m + n))
          FailedBy b m -> FailedBy b (max 0 (m + n))
        modifySkillTestResult r _ = r
      case modifiedSkillTestResult of
        SucceededBy _ n ->
          pushAll
            ( [ After
                ( PassedSkillTest
                    skillTestInvestigator
                    skillTestAction
                    skillTestSource
                    target
                    skillTestType
                    n
                )
              | target <- skillTestSubscribers
              ]
                <> [ After
                      ( PassedSkillTest
                          skillTestInvestigator
                          skillTestAction
                          skillTestSource
                          (SkillTestInitiatorTarget skillTestTarget)
                          skillTestType
                          n
                      )
                   ]
            )
        FailedBy _ n ->
          pushAll
            ( [ After
                ( FailedSkillTest
                    skillTestInvestigator
                    skillTestAction
                    skillTestSource
                    target
                    skillTestType
                    n
                )
              | target <- skillTestSubscribers
              ]
                <> [ After
                      ( FailedSkillTest
                          skillTestInvestigator
                          skillTestAction
                          skillTestSource
                          (SkillTestInitiatorTarget skillTestTarget)
                          skillTestType
                          n
                      )
                   ]
            )
        Unrun -> pure ()
      pure s
    SkillTestApplyResults -> do
      -- ST.7 Apply Results
      push SkillTestApplyResultsAfter
      modifiers' <- getModifiers (toTarget s)
      let
        successTimes = if DoubleSuccess `elem` modifiers' then 2 else 1
        modifiedSkillTestResult =
          foldl' modifySkillTestResult skillTestResult modifiers'
        modifySkillTestResult r (SkillTestResultValueModifier n) = case r of
          Unrun -> Unrun
          SucceededBy b m -> SucceededBy b (max 0 (m + n))
          FailedBy b m -> FailedBy b (max 0 (m + n))
        modifySkillTestResult r _ = r
      case modifiedSkillTestResult of
        SucceededBy _ n -> do
          pushAll
            $ [ When
                ( PassedSkillTest
                    skillTestInvestigator
                    skillTestAction
                    skillTestSource
                    target
                    skillTestType
                    n
                )
              | target <- skillTestSubscribers
              ]
            <> [ When
                  ( PassedSkillTest
                      skillTestInvestigator
                      skillTestAction
                      skillTestSource
                      (SkillTestInitiatorTarget skillTestTarget)
                      skillTestType
                      n
                  )
               ]
            <> ( cycleN
                  successTimes
                  ( [ PassedSkillTest
                      skillTestInvestigator
                      skillTestAction
                      skillTestSource
                      target
                      skillTestType
                      n
                    | target <- skillTestSubscribers
                    ]
                      <> [ PassedSkillTest
                            skillTestInvestigator
                            skillTestAction
                            skillTestSource
                            (SkillTestInitiatorTarget skillTestTarget)
                            skillTestType
                            n
                         ]
                  )
               )
        FailedBy _ n -> do
          hauntedAbilities <- case (skillTestTarget, skillTestAction) of
            (LocationTarget lid, Just Action.Investigate) -> selectList $ HauntedAbility <> AbilityOnLocation (LocationWithId lid)
            _ -> pure []
          pushAll
            $ [ When
                  ( FailedSkillTest
                      skillTestInvestigator
                      skillTestAction
                      skillTestSource
                      (SkillTestInitiatorTarget skillTestTarget)
                      skillTestType
                      n
                  )
              ]
            <> [ When
                ( FailedSkillTest
                    skillTestInvestigator
                    skillTestAction
                    skillTestSource
                    target
                    skillTestType
                    n
                )
               | target <- skillTestSubscribers
               ]
            <> [ FailedSkillTest
                skillTestInvestigator
                skillTestAction
                skillTestSource
                target
                skillTestType
                n
               | target <- skillTestSubscribers
               ]
            <> [ FailedSkillTest
                  skillTestInvestigator
                  skillTestAction
                  skillTestSource
                  (SkillTestInitiatorTarget skillTestTarget)
                  skillTestType
                  n
               ]
            <> [ chooseOneAtATime
                skillTestInvestigator
                [AbilityLabel skillTestInvestigator ab [] [] | ab <- hauntedAbilities]
               | notNull hauntedAbilities
               ]
        Unrun -> pure ()
      pure s
    RerunSkillTest -> case skillTestResult of
      FailedBy Automatic _ -> pure s
      _ -> do
        withQueue_ $ filter $ \case
          Will FailedSkillTest {} -> False
          Will PassedSkillTest {} -> False
          CheckWindow _ [Window Timing.When (Window.WouldFailSkillTest _) _] ->
            False
          CheckWindow _ [Window Timing.When (Window.WouldPassSkillTest _) _] ->
            False
          RunWindow _ [Window Timing.When (Window.WouldPassSkillTest _) _] ->
            False
          RunWindow _ [Window Timing.When (Window.WouldFailSkillTest _) _] ->
            False
          Ask skillTestInvestigator' (ChooseOne [SkillTestApplyResultsButton])
            | skillTestInvestigator == skillTestInvestigator' -> False
          _ -> True
        push $ RunSkillTest skillTestInvestigator
        -- We need to subtract the current token values to prevent them from
        -- doubling. However, we need to keep any existing value modifier on
        -- the stack (such as a token no longer visible who effect still
        -- persists)
        chaosTokenValues <-
          sum
            <$> for
              (skillTestRevealedChaosTokens <> skillTestResolvedChaosTokens)
              (getModifiedChaosTokenValue s)
        pure $ s & valueModifierL %~ subtract chaosTokenValues
    RecalculateSkillTestResults -> do
      results <- calculateSkillTestResultsData s
      push $ SkillTestResults results
      pure s
    RunSkillTest _ -> do
      results <- calculateSkillTestResultsData s
      push $ SkillTestResults results
      -- TODO: We should be able to get all of this from the results data, but
      -- there is a discrepancy between totaledTokenValues and the info stored
      -- in the result data, this may be incorrect, need to investigate
      chaosTokenValues <-
        sum
          <$> for
            (skillTestRevealedChaosTokens <> skillTestResolvedChaosTokens)
            (getModifiedChaosTokenValue s)
      let
        modifiedSkillValue' =
          max
            0
            (skillTestResultsSkillValue results + totaledChaosTokenValues + skillTestResultsIconValue results)
        totaledChaosTokenValues = chaosTokenValues + skillTestValueModifier
        result =
          if skillTestResultsSuccess results
            then SucceededBy NonAutomatic (modifiedSkillValue' - skillTestResultsDifficulty results)
            else FailedBy NonAutomatic (skillTestResultsDifficulty results - modifiedSkillValue')

      pure $ s & valueModifierL .~ totaledChaosTokenValues & resultL .~ result
    ChangeSkillTestType newSkillTestType newSkillTestBaseValue ->
      pure $ s & typeL .~ newSkillTestType & baseValueL .~ newSkillTestBaseValue
    RemoveAllChaosTokens face -> do
      pure
        $ s
        & revealedChaosTokensL
        %~ filter ((/= face) . chaosTokenFace)
        & setAsideChaosTokensL
        %~ filter ((/= face) . chaosTokenFace)
        & resolvedChaosTokensL
        %~ filter ((/= face) . chaosTokenFace)
    _ -> pure s
