{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.SkillTest.Runner
  ( module X
  , skillIconCount
  , getCurrentSkillValue
  ) where

import Arkham.Prelude

import Arkham.SkillTest as X

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.Classes
import Arkham.Game.Helpers hiding ( matches )
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator
import Arkham.Investigator.Types (Field(..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.RequestedTokenStrategy
import Arkham.SkillTestResult
import Arkham.SkillType
import Arkham.Source
import Arkham.Stats
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window
import Control.Lens (each)
import Data.HashMap.Strict qualified as HashMap

calculateSkillTestResultsData :: HasGame m => SkillTest -> m SkillTestResultsData
calculateSkillTestResultsData s = do
  modifiers' <- getModifiers SkillTestTarget
  modifiedSkillTestDifficulty <- getModifiedSkillTestDifficulty s
  iconCount <- if CancelSkills `elem` modifiers'
    then pure 0
    else skillIconCount s
  currentSkillValue <- getCurrentSkillValue s
  tokenValues <- sum <$> for
    (skillTestRevealedTokens s <> skillTestResolvedTokens s)
    (getModifiedTokenValue s)
  let
    addResultModifier n (SkillTestResultValueModifier m) = n + m
    addResultModifier n _ = n
    resultValueModifiers = foldl' addResultModifier 0 modifiers'
    totaledTokenValues = tokenValues + (skillTestValueModifier s)
    modifiedSkillValue' =
      max 0 (currentSkillValue + totaledTokenValues + iconCount)
    op = if FailTies `elem` modifiers' then (>) else (>=)
    isSuccess = modifiedSkillValue' `op` modifiedSkillTestDifficulty
  pure $ SkillTestResultsData
    currentSkillValue
    iconCount
    totaledTokenValues
    modifiedSkillTestDifficulty
    (resultValueModifiers <$ guard (resultValueModifiers /= 0))
    isSuccess

autoFailSkillTestResultsData :: HasGame m => SkillTest -> m SkillTestResultsData
autoFailSkillTestResultsData s = do
  modifiedSkillTestDifficulty <- getModifiedSkillTestDifficulty s
  tokenValues <- sum <$> for
    (skillTestRevealedTokens s <> skillTestResolvedTokens s)
    (getModifiedTokenValue s)
  let
    totaledTokenValues = tokenValues + (skillTestValueModifier s)
  pure $ SkillTestResultsData
    0
    0
    totaledTokenValues
    modifiedSkillTestDifficulty
    Nothing
    False

getCurrentSkillValue :: HasGame m => SkillTest -> m Int
getCurrentSkillValue st = case skillTestBaseValue st of
  SkillBaseValue sType -> do
    stats <- modifiedStatsOf (skillTestAction st) (skillTestInvestigator st)
    pure $ statsSkillValue stats sType
  HalfResourcesOf iid -> fieldMap InvestigatorResources (`div` 2) iid
  StaticBaseValue n -> pure n

skillIconCount :: HasGame m => SkillTest -> m Int
skillIconCount SkillTest {..} = do
  totalIcons <- length . filter matches <$> concatMapM
      iconsForCard
      (concat $ toList skillTestCommittedCards)
  case skillTestType of
    SkillSkillTest sType -> do
      investigatorModifiers <- getModifiers
        (InvestigatorTarget skillTestInvestigator)
      pure $ if SkillCannotBeIncreased sType `elem` investigatorModifiers
        then 0
        else totalIcons
    ResourceSkillTest -> pure totalIcons
 where
  matches WildIcon = True
  matches (SkillIcon s) = case skillTestType of
    SkillSkillTest sType -> s == sType
    ResourceSkillTest -> False

getModifiedSkillTestDifficulty :: HasGame m => SkillTest -> m Int
getModifiedSkillTestDifficulty s = do
  modifiers' <- getModifiers SkillTestTarget
  let
    preModifiedDifficulty =
      foldr applyPreModifier (skillTestDifficulty s) modifiers'
  pure $ foldr applyModifier preModifiedDifficulty modifiers'
 where
  applyModifier (Difficulty m) n = max 0 (n + m)
  applyModifier DoubleDifficulty n = n * 2
  applyModifier _ n = n
  applyPreModifier (SetDifficulty m) _ = m
  applyPreModifier _ n = n

-- per the FAQ the double negative modifier ceases to be active
-- when Sure Gamble is used so we overwrite both Negative and DoubleNegative
getModifiedTokenValue :: HasGame m => SkillTest -> Token -> m Int
getModifiedTokenValue s t = do
  tokenModifiers' <- getModifiers (TokenTarget t)
  modifiedTokenFaces' <- getModifiedTokenFaces [t]
  getSum . mconcat <$> for
    modifiedTokenFaces'
    (\tokenFace -> do
      baseTokenValue <- getTokenValue (skillTestInvestigator s) tokenFace ()
      let
        updatedTokenValue =
          tokenValue $ foldr applyModifier baseTokenValue tokenModifiers'
      pure . Sum $ fromMaybe 0 updatedTokenValue
    )
 where
  applyModifier IgnoreToken (TokenValue token _) = TokenValue token NoModifier
  applyModifier (ChangeTokenModifier modifier') (TokenValue token _) =
    TokenValue token modifier'
  applyModifier NegativeToPositive (TokenValue token (NegativeModifier n)) =
    TokenValue token (PositiveModifier n)
  applyModifier NegativeToPositive (TokenValue token (DoubleNegativeModifier n))
    = TokenValue token (PositiveModifier n)
  applyModifier DoubleNegativeModifiersOnTokens (TokenValue token (NegativeModifier n))
    = TokenValue token (DoubleNegativeModifier n)
  applyModifier (TokenValueModifier m) (TokenValue token (PositiveModifier n))
    = TokenValue token (PositiveModifier (max 0 (n + m)))
  applyModifier (TokenValueModifier m) (TokenValue token (NegativeModifier n))
    = TokenValue token (NegativeModifier (max 0 (n - m)))
  applyModifier _ currentTokenValue = currentTokenValue

instance RunMessage SkillTest where
  runMessage msg s@SkillTest {..} = case msg of
    Discard _ target | target == skillTestTarget -> do
      push $ SkillTestEnds skillTestInvestigator skillTestSource
      pure s
    RemoveFromGame target | target == skillTestTarget -> do
      push $ SkillTestEnds skillTestInvestigator skillTestSource
      pure s
    TriggerSkillTest iid -> do
      modifiers' <- getModifiers iid
      if DoNotDrawChaosTokensForSkillChecks `elem` modifiers'
        then do
          let
            tokensTreatedAsRevealed = flip mapMaybe modifiers' $ \case
              TreatRevealedTokenAs t -> Just t
              _ -> Nothing
          if null tokensTreatedAsRevealed
            then push (RunSkillTest iid)
            else do
              pushAll [RevealSkillTestTokens iid, RunSkillTest iid]
              for_ tokensTreatedAsRevealed $ \tokenFace -> do
                t <- getRandom
                pushAll
                  $ resolve (RevealToken (toSource s) iid (Token t tokenFace))
        else if SkillTestAutomaticallySucceeds `elem` modifiers'
          then pushAll [PassSkillTest, UnsetActiveCard]
          else do
            let
              applyRevealStategyModifier _ (ChangeRevealStrategy n) = n
              applyRevealStategyModifier n _ = n
              revealStrategy =
                foldl' applyRevealStategyModifier (Reveal 1) modifiers'
            pushAll
              [ RequestTokens (toSource s) (Just iid) revealStrategy SetAside
              , RunSkillTest iid
              ]
      pure s
    DrawAnotherToken iid -> do
      withQueue_ $ filter $ \case
        Will FailedSkillTest{} -> False
        Will PassedSkillTest{} -> False
        CheckWindow _ [Window Timing.When (Window.WouldFailSkillTest _)] ->
          False
        CheckWindow _ [Window Timing.When (Window.WouldPassSkillTest _)] ->
          False
        RunWindow _ [Window Timing.When (Window.WouldPassSkillTest _)] -> False
        RunWindow _ [Window Timing.When (Window.WouldFailSkillTest _)] -> False
        Ask skillTestInvestigator' (ChooseOne [SkillTestApplyResultsButton])
          | skillTestInvestigator == skillTestInvestigator' -> False
        _ -> True
      pushAll
        [ RequestTokens (toSource s) (Just iid) (Reveal 1) SetAside
        , RunSkillTest iid
        ]
      pure $ s & (resolvedTokensL %~ (<> skillTestRevealedTokens))
    RequestedTokens (SkillTestSource siid skillType source maction) (Just iid) tokenFaces
      -> do
        push (RevealSkillTestTokens iid)
        for_ tokenFaces $ \tokenFace -> do
          let
            revealMsg = RevealToken (SkillTestSource siid skillType source maction) iid tokenFace
          pushAll [When revealMsg, CheckWindow [iid] [Window Timing.AtIf (Window.RevealToken iid tokenFace)], revealMsg, After revealMsg]
        pure $ s & (setAsideTokensL %~ (tokenFaces <>))
    RevealToken SkillTestSource{} iid token -> do
      push
        (CheckWindow [iid] [Window Timing.After (Window.RevealToken iid token)])
      pure $ s & revealedTokensL %~ (token :)
    RevealSkillTestTokens iid -> do
      revealedTokenFaces <- flip
        concatMapM
        (skillTestRevealedTokens \\ skillTestResolvedTokens)
        \token -> do
          faces <- getModifiedTokenFaces [token]
          pure [ (token, face) | face <- faces ]
      pushAll
        [ ResolveToken drawnToken tokenFace iid
        | (drawnToken, tokenFace) <- revealedTokenFaces
        ]
      pure
        $ s
        & (subscribersL
          %~ (nub . (<> [ TokenTarget token' | token' <- skillTestRevealedTokens ]))
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
        ]
      pure $ s & resultL .~ SucceededBy True modifiedSkillValue'
    FailSkillTest -> do
      resultsData <- autoFailSkillTestResultsData s
      difficulty <- getModifiedSkillTestDifficulty s
      pushAll
        $ SkillTestResults resultsData
        : [ Will
              (FailedSkillTest
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
             (FailedSkillTest
               skillTestInvestigator
               skillTestAction
               skillTestSource
               (SkillTestInitiatorTarget skillTestTarget)
               skillTestType
               difficulty
             )
           , chooseOne skillTestInvestigator [SkillTestApplyResultsButton]
           , SkillTestEnds skillTestInvestigator skillTestSource
           ]
      pure $ s & resultL .~ FailedBy True difficulty
    StartSkillTest _ -> do
      windowMsg <- checkWindows [Window Timing.When Window.FastPlayerWindow]
      pushAll
        $ HashMap.foldMapWithKey
            (\i cs -> [CheckAdditionalCommitCosts i cs])
            skillTestCommittedCards
        <> [windowMsg, TriggerSkillTest skillTestInvestigator]
      pure s
    CheckAdditionalCommitCosts iid cards -> do
      modifiers' <- getModifiers (InvestigatorTarget iid)
      let
        msgs = [CommitCard iid card | card <- cards]
        additionalCosts = mapMaybe
          (\case
            CommitCost c -> Just c
            _ -> Nothing
          )
          modifiers'
      if null additionalCosts
        then pushAll msgs
        else do
          canPay <- getCanAffordCost
            iid
            (toSource s)
            Nothing
            [Window Timing.When Window.NonFast]
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
    PutCardIntoPlay _ card _ _ -> do
      pure $ s & committedCardsL %~ map (filter (/= card))
    CardEnteredPlay _ card -> do
      pure $ s & committedCardsL %~ map (filter (/= card))
    SkillTestCommitCard iid card -> do
      pure $ s & committedCardsL %~ insertWith (<>) iid [card]
    SkillTestUncommitCard _ card ->
      pure $ s & committedCardsL %~ map (filter (/= card))
    ReturnSkillTestRevealedTokens -> do
      -- Rex's Curse timing keeps effects on stack so we do
      -- not want to remove them as subscribers from the stack
      push $ ResetTokens (toSource s)
      pure
        $ s
        & (setAsideTokensL .~ mempty)
        & (revealedTokensL .~ mempty)
        & (resolvedTokensL .~ mempty)
        & (valueModifierL .~ 0)
    SkillTestEnds _ _ -> do
      -- Skill Cards are in the environment and will be discarded normally
      -- However, all other cards need to be discarded here.
      let
        discards = concatMap
          (\case
            (iid, cards) -> flip mapMaybe cards $ \case
              PlayerCard pc -> (iid, pc) <$ guard (cdCardType (toCardDef pc) /= SkillType)
              EncounterCard _ -> Nothing
              VengeanceCard _ -> Nothing
          )
          (s ^. committedCardsL . to mapToList)

      skillTestEndsWindows <- windows [Window.SkillTestEnded s]
      pushAll
        $ ResetTokens (toSource s)
        : map (uncurry AddToDiscard) discards
        <> skillTestEndsWindows
        <> [ AfterSkillTestEnds
               skillTestSource
               skillTestTarget
               skillTestResult
           ]
      pure s
    ReturnToHand _ (CardIdTarget cardId) -> do
      liftIO $ do
        print skillTestCommittedCards
        print $ skillTestCommittedCards & each %~ filter ((/= cardId) . toCardId)
      pure $ s & committedCardsL . each %~ filter ((/= cardId) . toCardId)
    ReturnToHand _ (CardTarget card) -> do
      pure $ s & committedCardsL . each %~ filter (/= card)
    SkillTestResults{} -> do
      modifiers' <- getModifiers (toTarget s)
      -- We may be recalculating so we want to remove all windows an buttons to apply
      removeAllMessagesMatching $ \case
        Will (PassedSkillTest{}) -> True
        Will (FailedSkillTest{}) -> True
        Ask _ (ChooseOne [ SkillTestApplyResultsButton ]) -> True
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
        SucceededBy _ n -> pushAll
          ([ Will
               (PassedSkillTest
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
                 (PassedSkillTest
                   skillTestInvestigator
                   skillTestAction
                   skillTestSource
                   (SkillTestInitiatorTarget skillTestTarget)
                   skillTestType
                   n
                 )
             ]
          )
        FailedBy _ n -> pushAll
          ([ Will
               (FailedSkillTest
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
                 (FailedSkillTest
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
      push $ SkillTestEnds skillTestInvestigator skillTestSource -- -> ST.8 -- Skill test ends
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
        SucceededBy _ n -> pushAll
          ([ After
               (PassedSkillTest
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
                 (PassedSkillTest
                   skillTestInvestigator
                   skillTestAction
                   skillTestSource
                   (SkillTestInitiatorTarget skillTestTarget)
                   skillTestType
                   n
                 )
             ]
          )
        FailedBy _ n -> pushAll
          ([ After
               (FailedSkillTest
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
                 (FailedSkillTest
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
                  (PassedSkillTest
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
                   (PassedSkillTest
                     skillTestInvestigator
                     skillTestAction
                     skillTestSource
                     (SkillTestInitiatorTarget skillTestTarget)
                     skillTestType
                     n
                   )
               ]
            <> (cycleN
                 successTimes
                 ([ PassedSkillTest
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
                 (FailedSkillTest
                   skillTestInvestigator
                   skillTestAction
                   skillTestSource
                   (SkillTestInitiatorTarget skillTestTarget)
                   skillTestType
                   n
                 )
             ]
            <> [ When
                   (FailedSkillTest
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
            <> [chooseOneAtATime skillTestInvestigator [AbilityLabel skillTestInvestigator ab [] [] | ab <- hauntedAbilities] | notNull hauntedAbilities ]
        Unrun -> pure ()
      pure s
    RerunSkillTest -> case skillTestResult of
      FailedBy True _ -> pure s
      _ -> do
        withQueue_ $ filter $ \case
          Will FailedSkillTest{} -> False
          Will PassedSkillTest{} -> False
          CheckWindow _ [Window Timing.When (Window.WouldFailSkillTest _)] ->
            False
          CheckWindow _ [Window Timing.When (Window.WouldPassSkillTest _)] ->
            False
          RunWindow _ [Window Timing.When (Window.WouldPassSkillTest _)] ->
            False
          RunWindow _ [Window Timing.When (Window.WouldFailSkillTest _)] ->
            False
          Ask skillTestInvestigator' (ChooseOne [SkillTestApplyResultsButton])
            | skillTestInvestigator == skillTestInvestigator' -> False
          _ -> True
        push (RunSkillTest skillTestInvestigator)
        -- We need to subtract the current token values to prevent them from
        -- doubling. However, we need to keep any existing value modifier on
        -- the stack (such as a token no longer visible who effect still
        -- persists)
        tokenValues <- sum <$> for
          (skillTestRevealedTokens <> skillTestResolvedTokens)
          (getModifiedTokenValue s)
        pure $ s & valueModifierL %~ subtract tokenValues
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
      tokenValues <- sum <$> for
        (skillTestRevealedTokens <> skillTestResolvedTokens)
        (getModifiedTokenValue s)
      let
        modifiedSkillValue' =
          max 0 (skillTestResultsSkillValue results + totaledTokenValues + skillTestResultsIconValue results)

        totaledTokenValues = tokenValues + skillTestValueModifier
      if skillTestResultsSuccess results
        then
          pure
          $ s
          & (resultL .~ SucceededBy
              False
              (modifiedSkillValue' - skillTestResultsDifficulty results)
            )
          & (valueModifierL .~ totaledTokenValues)
        else
          pure
          $ s
          & (resultL .~ FailedBy
              False
              (skillTestResultsDifficulty results - modifiedSkillValue')
            )
          & (valueModifierL .~ totaledTokenValues)
    ChangeSkillTestType newSkillTestType newSkillTestBaseValue ->
      pure $ s & typeL .~ newSkillTestType & baseValueL .~ newSkillTestBaseValue
    _ -> pure s
