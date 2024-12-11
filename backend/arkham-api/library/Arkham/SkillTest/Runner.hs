{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.SkillTest.Runner (module X, totalModifiedSkillValue) where

import Arkham.Prelude

import Arkham.SkillTest as X

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Calculation
import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes hiding (matches)
import Arkham.Classes.HasGame
import Arkham.Deck qualified as Deck
import Arkham.Game.Helpers
import Arkham.Helpers.Card
import Arkham.Helpers.Message
import Arkham.Matcher hiding (IgnoreChaosToken, RevealChaosToken)
import Arkham.Message qualified as Msg
import Arkham.Projection
import Arkham.RequestedChaosTokenStrategy
import Arkham.Skill.Types as Field
import Arkham.SkillTest.Step
import Arkham.SkillTestResult
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..), mkAfter, mkWhen, mkWindow)
import Arkham.Window qualified as Window
import Control.Lens (each)
import Data.Map.Strict qualified as Map

totalChaosTokenValues :: HasGame m => SkillTest -> m Int
totalChaosTokenValues s = do
  x <- sum <$> for (skillTestSetAsideChaosTokens s) (getModifiedChaosTokenValue s)
  y <- getAdditionalChaosTokenValues s
  pure $ x + y

totalModifiedSkillValue :: HasGame m => SkillTest -> m Int
totalModifiedSkillValue s = do
  results <- calculateSkillTestResultsData s
  chaosTokenValues <- totalChaosTokenValues s

  pure
    $ max
      0
      (skillTestResultsSkillValue results + chaosTokenValues + skillTestResultsIconValue results)

calculateSkillTestResultsData :: HasGame m => SkillTest -> m SkillTestResultsData
calculateSkillTestResultsData s = do
  modifiers' <- getModifiers (SkillTestTarget s.id)
  modifiedSkillTestDifficulty <- getModifiedSkillTestDifficulty s
  let cancelSkills = CancelSkills `elem` modifiers'
  iconCount <- if cancelSkills then pure 0 else skillIconCount s
  subtractIconCount <- if cancelSkills then pure 0 else subtractSkillIconCount s
  currentSkillValue <- getCurrentSkillValue s
  chaosTokenValues <- totalChaosTokenValues s
  let
    addResultModifier n (SkillTestResultValueModifier m) = n + m
    addResultModifier n _ = n
    resultValueModifiers = foldl' addResultModifier 0 modifiers'
    modifiedSkillValue' =
      max 0 (currentSkillValue + chaosTokenValues + iconCount - subtractIconCount)
    op = if FailTies `elem` modifiers' then (>) else (>=)
    isSuccess = modifiedSkillValue' `op` modifiedSkillTestDifficulty
  pure
    $ SkillTestResultsData
      currentSkillValue
      (iconCount - subtractIconCount)
      chaosTokenValues
      modifiedSkillTestDifficulty
      (resultValueModifiers <$ guard (resultValueModifiers /= 0))
      isSuccess

autoFailSkillTestResultsData :: HasGame m => SkillTest -> m SkillTestResultsData
autoFailSkillTestResultsData s = do
  modifiedSkillTestDifficulty <- getModifiedSkillTestDifficulty s
  mods <- getModifiers s
  let x = getSum $ mconcat [Sum n | SkillTestResultValueModifier n <- mods]
  pure $ SkillTestResultsData 0 0 0 modifiedSkillTestDifficulty (guard (x /= 0) $> x) False

subtractSkillIconCount :: HasGame m => SkillTest -> m Int
subtractSkillIconCount SkillTest {..} =
  count matches <$> concatMapM iconsForCard (concat $ toList skillTestCommittedCards)
 where
  matches WildMinusIcon = True
  matches WildIcon = False
  matches (SkillIcon _) = False

getAdditionalChaosTokenValues :: HasGame m => SkillTest -> m Int
getAdditionalChaosTokenValues s = do
  mods <- getModifiers s
  let vs = [v | AddChaosTokenValue v <- mods]
  getSum <$> foldMapM (fmap (Sum . fromMaybe 0) . chaosTokenValue) vs

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
          updatedChaosTokenValue <- chaosTokenValue (foldr applyModifier baseChaosTokenValue tokenModifiers')
          pure . Sum $ fromMaybe 0 updatedChaosTokenValue
      )
      modifiedChaosTokenFaces'
 where
  applyModifier IgnoreChaosTokenEffects (ChaosTokenValue token _) = ChaosTokenValue token NoModifier
  applyModifier IgnoreChaosToken (ChaosTokenValue token _) = ChaosTokenValue token NoModifier
  applyModifier IgnoreChaosTokenModifier (ChaosTokenValue token _) = ChaosTokenValue token NoModifier
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
    RepeatSkillTest sid skillTestId' | skillTestId' == skillTestId -> do
      push
        $ BeginSkillTestWithPreMessages' []
        $ ( buildSkillTest
              sid
              skillTestInvestigator
              skillTestSource
              skillTestTarget
              skillTestType
              skillTestBaseValue
              skillTestDifficulty
          )
          { skillTestAction = skillTestAction
          }
      pure s
    IncreaseSkillTestDifficulty n -> do
      -- see: faqs/drawing-thin
      pure $ s & difficultyL %~ \(SkillTestDifficulty d) -> SkillTestDifficulty (SumCalculation [d, Fixed n])
    ReturnChaosTokens tokens -> do
      pure
        $ s
        & (setAsideChaosTokensL %~ filter (`notElem` tokens))
        & (revealedChaosTokensL %~ filter (`notElem` tokens))
        & (resolvedChaosTokensL %~ filter (`notElem` tokens))
        & (toResolveChaosTokensL %~ filter (`notElem` tokens))
    BeforeSkillTest stId | stId == s.id -> do
      pure $ s & stepL .~ CommitCardsFromHandToSkillTestStep
    BeginSkillTestAfterFast -> do
      windowMsg <- checkWindows [mkWindow #when Window.FastPlayerWindow]
      pushAll [windowMsg, BeforeSkillTest s.id, EndSkillTestWindow]
      mAbilityCardId <- case skillTestSource of
        AbilitySource src _ -> fmap toCardId <$> sourceToMaybeCard src
        t -> fmap toCardId <$> sourceToMaybeCard t
      mTargetCardId <- case skillTestTarget of
        ProxyTarget t _ -> fmap toCardId <$> targetToMaybeCard t
        t -> fmap toCardId <$> targetToMaybeCard t
      mSourceCardId <- case skillTestSource of
        ProxySource _ t -> fmap toCardId <$> sourceToMaybeCard t
        IndexedSource _ t -> fmap toCardId <$> sourceToMaybeCard t
        AbilitySource src _ -> fmap toCardId <$> sourceToMaybeCard src
        t -> fmap toCardId <$> sourceToMaybeCard t

      -- getAlternateSkill :: HasGame m => SkillTest -> SkillType -> m SkillType

      updatedSkillTestType <- case skillTestType of
        SkillSkillTest stype -> SkillSkillTest <$> getAlternateSkill s stype
        AndSkillTest sks -> AndSkillTest <$> traverse (getAlternateSkill s) sks
        x@BaseValueSkillTest {} -> pure x
        x@ResourceSkillTest -> pure x

      updatedBaseValue <- case skillTestBaseValue of
        SkillBaseValue stype -> SkillBaseValue <$> getAlternateSkill s stype
        AndSkillBaseValue sks -> AndSkillBaseValue <$> traverse (getAlternateSkill s) sks
        x@HalfResourcesOf {} -> pure x
        x@FixedBaseValue {} -> pure x

      let icons =
            if null skillTestIconValues
              then iconValuesForSkillTestType updatedSkillTestType
              else skillTestIconValues

      pure
        $ s
        & (targetCardL .~ mTargetCardId)
        & (sourceCardL .~ (mAbilityCardId <|> mSourceCardId))
        & (stepL .~ SkillTestFastWindow1)
        & (skillTestTypeL .~ updatedSkillTestType)
        & (iconValuesL .~ icons)
        & (baseValueL .~ updatedBaseValue)
    ReplaceSkillTestSkill (FromSkillType fsType) (ToSkillType tsType) -> do
      let
        stType = case skillTestType of
          ResourceSkillTest -> ResourceSkillTest
          x@(BaseValueSkillTest _ _) -> x
          SkillSkillTest currentType -> if currentType == fsType then SkillSkillTest tsType else SkillSkillTest currentType
          AndSkillTest types -> AndSkillTest $ map (\t -> if t == fsType then tsType else t) types
        stBaseValue = case skillTestBaseValue of
          SkillBaseValue currentType -> SkillBaseValue $ if currentType == fsType then tsType else currentType
          AndSkillBaseValue xs -> AndSkillBaseValue $ map (\t -> if t == fsType then tsType else t) xs
          HalfResourcesOf x -> HalfResourcesOf x
          FixedBaseValue x -> FixedBaseValue x

      pure
        $ s
          { skillTestType = stType
          , skillTestBaseValue = stBaseValue
          , skillTestIconValues = iconValuesForSkillTestType stType
          }
    SetSkillTestTarget target -> do
      pure $ s {skillTestTarget = target}
    Discard _ _ target | target == skillTestTarget -> do
      when (skillTestStep < RevealChaosTokenStep) do
        pushAll
          [ SkillTestEnds skillTestId skillTestInvestigator skillTestSource
          , Do (SkillTestEnds skillTestId skillTestInvestigator skillTestSource)
          ]
      pure s
    RemoveFromGame target | target == skillTestTarget -> do
      when (skillTestStep < RevealChaosTokenStep) do
        pushAll
          [ SkillTestEnds skillTestId skillTestInvestigator skillTestSource
          , Do (SkillTestEnds skillTestId skillTestInvestigator skillTestSource)
          ]
      pure s
    TriggerSkillTest iid -> do
      modifiers' <- getModifiers iid
      modifiers'' <- getModifiers (SkillTestTarget skillTestId)
      if DoNotDrawChaosTokensForSkillChecks `elem` modifiers'
        then do
          let
            tokensTreatedAsRevealed = flip mapMaybe modifiers' $ \case
              TreatRevealedChaosTokenAs t -> Just t
              _ -> Nothing
          if null tokensTreatedAsRevealed
            then push (RunSkillTest iid)
            else do
              pushAll
                [ When (RevealSkillTestChaosTokens iid)
                , RevealSkillTestChaosTokens iid
                , RunSkillTest iid
                ]
              for_ tokensTreatedAsRevealed $ \chaosTokenFace -> do
                t <- getRandom
                pushAll
                  $ resolve (RevealChaosToken (toSource s) iid (ChaosToken t chaosTokenFace (Just iid)))
        else
          if SkillTestAutomaticallySucceeds `elem` modifiers''
            then pushAll [PassSkillTest, UnsetActiveCard]
            else do
              let
                applyRevealStategyModifier (MultiReveal _ b) (ChangeRevealStrategy n) = MultiReveal n b
                applyRevealStategyModifier _ (ChangeRevealStrategy n) = n
                applyRevealStategyModifier n RevealAnotherChaosToken = MultiReveal n (Reveal 1)
                applyRevealStategyModifier n _ = n
                revealStrategy =
                  foldl' applyRevealStategyModifier (Reveal 1) (modifiers' <> modifiers'')
              pushAll
                [ RequestChaosTokens (toSource s) (Just iid) revealStrategy SetAside
                , RunSkillTest iid
                ]
      pure s
    DrawAnotherChaosToken iid -> do
      player <- getPlayer skillTestInvestigator
      withQueue_ $ filter $ \case
        Will FailedSkillTest {} -> False
        Will PassedSkillTest {} -> False
        CheckWindows [Window Timing.When (Window.WouldFailSkillTest _) _] ->
          False
        CheckWindows [Window Timing.When (Window.WouldPassSkillTest _) _] ->
          False
        Do (CheckWindows [Window Timing.When (Window.WouldFailSkillTest _) _]) ->
          False
        Do (CheckWindows [Window Timing.When (Window.WouldPassSkillTest _) _]) ->
          False
        Ask player' (ChooseOne [SkillTestApplyResultsButton])
          | player == player' -> False
        _ -> True
      pushAll
        [ RequestChaosTokens (toSource s) (Just iid) (Reveal 1) SetAside
        , RunSkillTest iid
        ]
      pure s
    RequestedChaosTokens (SkillTestSource sid) (Just iid) chaosTokenFaces -> do
      skillTestModifiers' <- getModifiers (SkillTestTarget sid)
      windowMsg <- checkWindows [mkWhen Window.FastPlayerWindow]
      popMessageMatching_ $ \case
        RevealSkillTestChaosTokens _ -> True
        _ -> False
      push
        $ if RevealChaosTokensBeforeCommittingCards `elem` skillTestModifiers'
          then
            CommitToSkillTest
              s.id
              (Label "Done Comitting" [CheckAllAdditionalCommitCosts, windowMsg, RevealSkillTestChaosTokens iid])
          else RevealSkillTestChaosTokens iid
      for_ chaosTokenFaces $ \chaosTokenFace -> do
        let revealMsg = RevealChaosToken (SkillTestSource sid) iid chaosTokenFace
        pushAll
          [ When revealMsg
          , CheckWindows [mkWindow Timing.AtIf (Window.RevealChaosToken iid chaosTokenFace)]
          , revealMsg
          , After revealMsg
          ]
      pure $ s & (setAsideChaosTokensL %~ (<> chaosTokenFaces))
    RevealChaosToken SkillTestSource {} _iid token -> do
      pure
        $ s
        & (revealedChaosTokensL %~ (<> [token]))
        & (toResolveChaosTokensL %~ nub . (<> [token]))
        & (setAsideChaosTokensL %~ nub . (<> [token]))
    RevealSkillTestChaosTokens iid -> do
      -- NOTE: this exists here because of Sacred Covenant (2), we want to
      -- cancel the modifiers but retain the effects so the effects are queued,
      -- but if the token is returned it will no longer be counted. If we need
      -- to move this window we will need an alternate solution.
      afterRevealMsg <- checkWindows [mkAfter $ Window.SkillTestStep RevealChaosTokenStep]
      afterResolveMsg <- checkWindows [mkAfter $ Window.SkillTestStep ResolveChaosSymbolEffectsStep]
      revealedChaosTokenFaces <- flip
        concatMapM
        skillTestToResolveChaosTokens
        \token -> do
          faces <- getModifiedChaosTokenFaces [token]
          pure [(token, face) | face <- faces]
      afterRevealWindow <-
        checkAfter $ Window.RevealChaosTokensDuringSkillTest iid s skillTestToResolveChaosTokens
      pushAll $ UnfocusChaosTokens
        : afterRevealMsg
        : afterRevealWindow
        : afterRevealMsg
        : [ Will (ResolveChaosToken drawnChaosToken chaosTokenFace iid)
          | (drawnChaosToken, chaosTokenFace) <- revealedChaosTokenFaces
          ]
          <> [afterResolveMsg]
      pure
        $ s
        & (toResolveChaosTokensL .~ mempty)
        & (resolvedChaosTokensL <>~ skillTestToResolveChaosTokens)
        & (stepL .~ ResolveChaosSymbolEffectsStep)
    RevealSkillTestChaosTokensAgain iid -> do
      revealedChaosTokenFaces <- flip
        concatMapM
        skillTestToResolveChaosTokens
        \token -> do
          faces <- getModifiedChaosTokenFaces [token]
          pure [(token, face) | face <- faces]
      afterRevealWindow <-
        checkAfter $ Window.RevealChaosTokensDuringSkillTest iid s skillTestToResolveChaosTokens
      pushAll
        $ afterRevealWindow
        : [ Will (ResolveChaosToken drawnChaosToken chaosTokenFace iid)
          | (drawnChaosToken, chaosTokenFace) <- revealedChaosTokenFaces
          ]
      pure $ s & toResolveChaosTokensL .~ mempty & resolvedChaosTokensL <>~ skillTestToResolveChaosTokens
    PassSkillTest -> do
      modifiedSkillValue' <- totalModifiedSkillValue s
      player <- getPlayer skillTestInvestigator
      removeAllMessagesMatching \case
        Ask _ (ChooseOne [SkillTestApplyResultsButton]) -> True
        _ -> False
      push $ chooseOne player [SkillTestApplyResultsButton]
      pure
        $ s
        & (resultL .~ SucceededBy Automatic modifiedSkillValue')
        & (difficultyL .~ SkillTestDifficulty (Fixed 0))
    PassSkillTestBy n -> do
      player <- getPlayer skillTestInvestigator
      removeAllMessagesMatching \case
        Ask _ (ChooseOne [SkillTestApplyResultsButton]) -> True
        _ -> False
      push $ chooseOne player [SkillTestApplyResultsButton]
      pure $ s & resultL .~ SucceededBy NonAutomatic n
    FailSkillTest -> do
      resultsData <- autoFailSkillTestResultsData s
      difficulty <- getModifiedSkillTestDifficulty s
      -- player <- getPlayer skillTestInvestigator
      investigatorsToResolveFailure <-
        (`notNullOr` [skillTestInvestigator])
          <$> select (InvestigatorWithModifier ResolvesFailedEffects)

      tokenSubscribers <- concatForM skillTestRevealedChaosTokens \token -> do
        faces <- getModifiedChaosTokenFaces [token]
        pure
          [ ChaosTokenTarget (token {chaosTokenFace = face})
          | face <- faces
          ]

      let needsChoice = skillTestResolveFailureInvestigator `notElem` investigatorsToResolveFailure
      let
        handleChoice resolver player =
          let failed target = FailedSkillTest resolver skillTestAction skillTestSource target skillTestType difficulty
           in SkillTestResults resultsData
                : [Will (failed target) | target <- skillTestSubscribers <> tokenSubscribers]
                  <> [ Will (failed (SkillTestInitiatorTarget skillTestTarget))
                     , chooseOne player [SkillTestApplyResultsButton]
                     , SkillTestEnds skillTestId resolver skillTestSource
                     , Do (SkillTestEnds skillTestId resolver skillTestSource)
                     ]

      if needsChoice
        then do
          resolversWithPlayers <- traverse (traverseToSnd getPlayer) investigatorsToResolveFailure
          lead <- getLeadPlayer

          push
            $ chooseOrRunOne
              lead
              [ targetLabel
                resolver
                $ SetSkillTestResolveFailureInvestigator resolver
                : handleChoice resolver player
              | (resolver, player) <- resolversWithPlayers
              ]
        else do
          player <- getPlayer skillTestResolveFailureInvestigator
          pushAll $ handleChoice skillTestResolveFailureInvestigator player
      pure $ s & resultL .~ FailedBy Automatic difficulty
    StartSkillTest _ -> do
      windowMsg <- checkWindows [mkWhen Window.FastPlayerWindow]
      pushAll [CheckAllAdditionalCommitCosts, windowMsg, TriggerSkillTest skillTestInvestigator]
      pure $ s & stepL .~ SkillTestFastWindow2
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
      afterMsg <- checkWindows [mkAfter $ Window.CommittedCards iid cards]
      if null additionalCosts
        then pushAll $ msgs <> [afterMsg]
        else do
          canPay <- getCanAffordCost iid (toSource s) [] [mkWhen Window.NonFast] (mconcat additionalCosts)
          iid' <- getActiveInvestigatorId
          when canPay
            $ pushAll
            $ [SetActiveInvestigator iid | iid /= iid']
            <> [ PayForAbility
                  (abilityEffect (SourceableWithCardCode (CardCode "skilltest") s) [] $ mconcat additionalCosts)
                  []
               ]
            <> [SetActiveInvestigator iid' | iid /= iid']
            <> msgs
            <> [afterMsg]
      pure s
    InvestigatorCommittedSkill _ skillId ->
      pure $ s & subscribersL %~ (nub . (SkillTarget skillId :))
    PutCardOnBottomOfDeck _ _ card -> do
      pure $ s & committedCardsL %~ map (filter (/= card))
    PutCardOnTopOfDeck _ _ card -> do
      pure $ s & committedCardsL %~ map (filter (/= card))
    PutCardIntoPlay _ card _ _ _ -> do
      pure $ s & committedCardsL %~ map (filter (/= card))
    CardEnteredPlay _ card -> do
      pure $ s & committedCardsL %~ map (filter (/= card))
    SkillTestCommitCard iid card -> do
      when (skillTestStep > CommitCardsFromHandToSkillTestStep) do
        push $ CheckAdditionalCommitCosts iid [card]
      pure $ s & committedCardsL %~ insertWith (<>) iid [card]
    CommitCard iid card | card `notElem` findWithDefault [] iid skillTestCommittedCards -> do
      pure $ s & committedCardsL %~ insertWith (<>) iid [card]
    SkillTestUncommitCard _ card ->
      pure $ s & committedCardsL %~ map (filter (/= card))
    ReturnSkillTestRevealedChaosTokens -> do
      -- Rex's Curse timing keeps effects on stack so we do
      -- not want to remove them as subscribers from the stack
      push $ ResetChaosTokens (toSource s)
      pure $ s & (setAsideChaosTokensL .~ mempty)
    AddToVictory (SkillTarget sid) -> do
      card <- field Field.SkillCard sid
      pure $ s & committedCardsL . each %~ filter (/= card)
    Do (SkillTestEnds _ _ _) -> do
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

        resultF =
          case skillTestResult of
            SucceededBy {} -> \case
              IfSuccessfulModifier m -> m
              other -> other
            FailedBy {} -> \case
              IfFailureModifier m -> m
              other -> other
            _ -> id

      discardMessages <- forMaybeM discards $ \(iid, discard) -> do
        mods <- map resultF <$> getModifiers (toCardId discard)
        let mDevourer = listToMaybe [iid' | SetAfterPlay (DevourThis iid') <- mods]
        pure
          $ if
            | Just iid' <- mDevourer ->
                Just (Run [ObtainCard discard.id, Devoured iid' $ toCard discard])
            | PlaceOnBottomOfDeckInsteadOfDiscard `elem` mods ->
                Just (PutCardOnBottomOfDeck iid (Deck.InvestigatorDeck iid) (toCard discard))
            | ReturnToHandAfterTest `elem` mods -> Just $ AddToHand iid [toCard discard]
            | otherwise -> guard (LeaveCardWhereItIs `notElem` mods) $> AddToDiscard iid discard

      pushAll
        $ ResetChaosTokens (toSource s)
        : discardMessages
          <> windows [Window.SkillTestEnded s]
          <> [ AfterSkillTestEnds skillTestSource skillTestTarget skillTestResult
             , Msg.SkillTestEnded skillTestId
             ]
      pure s
    ReturnToHand _ (SkillTarget sid) -> do
      card <- field Field.SkillCard sid
      pure $ s & committedCardsL . each %~ filter ((/= card.id) . toCardId)
    ReturnToHand _ (CardIdTarget cardId) -> do
      pure $ s & committedCardsL . each %~ filter ((/= cardId) . toCardId)
    SkillTestResults {} -> do
      modifiers' <- getModifiers (toTarget s)
      -- We may be recalculating so we want to remove all windows an buttons to apply
      removeAllMessagesMatching $ \case
        Will (PassedSkillTest {}) -> True
        Will (FailedSkillTest {}) -> True
        Ask _ (ChooseOne [SkillTestApplyResultsButton]) -> True
        _ -> False
      player <- getPlayer skillTestInvestigator
      push (chooseOne player [SkillTestApplyResultsButton])
      let
        modifiedSkillTestResult =
          foldl' modifySkillTestResult skillTestResult modifiers'
        modifySkillTestResult r (SkillTestResultValueModifier n) = case r of
          Unrun -> Unrun
          SucceededBy b m -> SucceededBy b (max 0 (m + n))
          FailedBy b m -> FailedBy b (max 0 (m + n))
        modifySkillTestResult r _ = r

      tokenSubscribers <- concatForM skillTestRevealedChaosTokens \token -> do
        faces <- getModifiedChaosTokenFaces [token]
        pure
          [ ChaosTokenTarget (token {chaosTokenFace = face})
          | face <- faces
          ]

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
              | target <- skillTestSubscribers <> tokenSubscribers
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
        FailedBy _ n -> do
          investigatorsToResolveFailure <-
            (`notNullOr` [skillTestInvestigator])
              <$> select (InvestigatorWithModifier ResolvesFailedEffects)

          let needsChoice = skillTestResolveFailureInvestigator `notElem` investigatorsToResolveFailure

          let
            handleChoice resolver =
              [ Will
                ( FailedSkillTest
                    resolver
                    skillTestAction
                    skillTestSource
                    target
                    skillTestType
                    n
                )
              | target <- skillTestSubscribers <> tokenSubscribers
              ]
                <> [ Will
                      ( FailedSkillTest
                          resolver
                          skillTestAction
                          skillTestSource
                          (SkillTestInitiatorTarget skillTestTarget)
                          skillTestType
                          n
                      )
                   ]

          if needsChoice
            then do
              lead <- getLeadPlayer
              push
                $ chooseOrRunOne
                  lead
                  [ targetLabel resolver $ SetSkillTestResolveFailureInvestigator resolver : handleChoice resolver
                  | resolver <- investigatorsToResolveFailure
                  ]
            else pushAll $ handleChoice skillTestResolveFailureInvestigator
        Unrun -> pure ()
      pure s
    SkillTestApplyResultsAfter -> do
      -- ST.7 -- apply results

      valid <- assertQueue \case
        SkillTestEnds {} -> True
        _ -> False

      -- If we haven't already decided to end the skill test we need to end it
      unless valid
        $ pushAll
          [ SkillTestEnds skillTestId skillTestInvestigator skillTestSource
          , Do (SkillTestEnds skillTestId skillTestInvestigator skillTestSource) -- -> ST.8 -- Skill test ends
          ]

      modifiers' <- getModifiers (toTarget s)
      let
        successTimes = if DoubleSuccess `elem` modifiers' then 2 else 1
        modifiedSkillTestResult =
          foldl' modifySkillTestResult skillTestResult modifiers'
        modifySkillTestResult r (SkillTestResultValueModifier n) = case r of
          Unrun -> Unrun
          SucceededBy b m -> SucceededBy b (max 0 (m + n))
          FailedBy b m -> FailedBy b (max 0 (m - n))
        modifySkillTestResult r _ = r
      tokenSubscribers <- concatForM skillTestRevealedChaosTokens \token -> do
        faces <- getModifiedChaosTokenFaces [token]
        pure [ChaosTokenTarget (token {chaosTokenFace = face}) | face <- faces]
      case modifiedSkillTestResult of
        SucceededBy _ n -> do
          let passed target = PassedSkillTest skillTestInvestigator skillTestAction skillTestSource target skillTestType n
          pushAll
            $ cycleN
              successTimes
              ( [passed target | target <- skillTestSubscribers <> tokenSubscribers]
                  <> [passed (SkillTestInitiatorTarget skillTestTarget)]
              )
        FailedBy _ n -> do
          investigatorsToResolveFailure <-
            (`notNullOr` [skillTestInvestigator])
              <$> select (InvestigatorWithModifier ResolvesFailedEffects)

          let needsChoice = skillTestResolveFailureInvestigator `notElem` investigatorsToResolveFailure
          let
            handleChoice resolver =
              let failed target = FailedSkillTest resolver skillTestAction skillTestSource target skillTestType n
               in [failed target | target <- skillTestSubscribers <> tokenSubscribers]
                    <> [failed (Initiator skillTestTarget)]

          targetMods <- getModifiers skillTestTarget
          let cancelled = CancelEffects `elem` modifiers' && EffectsCannotBeCanceled `notElem` targetMods
          unless cancelled do
            if needsChoice
              then do
                lead <- getLeadPlayer

                push
                  $ chooseOrRunOne
                    lead
                    [ targetLabel resolver
                      $ SetSkillTestResolveFailureInvestigator resolver
                      : handleChoice resolver
                    | resolver <- investigatorsToResolveFailure
                    ]
              else pushAll $ handleChoice skillTestResolveFailureInvestigator
        Unrun -> pure ()

      pure $ s & stepL .~ ApplySkillTestResultsStep
    SkillTestApplyResults -> do
      -- ST.6 Determin Success
      push SkillTestApplyResultsAfter
      modifiers' <- getModifiers (toTarget s)
      let
        modifiedSkillTestResult =
          foldl' modifySkillTestResult skillTestResult modifiers'
        modifySkillTestResult r (SkillTestResultValueModifier n) = case r of
          Unrun -> Unrun
          SucceededBy b m -> SucceededBy b (max 0 (m + n))
          FailedBy b m -> FailedBy b (max 0 (m + n))
        modifySkillTestResult r _ = r

      tokenSubscribers <- concatForM skillTestRevealedChaosTokens \token -> do
        faces <- getModifiedChaosTokenFaces [token]
        pure [ChaosTokenTarget (token {chaosTokenFace = face}) | face <- faces]
      case modifiedSkillTestResult of
        SucceededBy _ n -> do
          let passed target = PassedSkillTest skillTestInvestigator skillTestAction skillTestSource target skillTestType n
          pushAll
            $ [When (passed target) | target <- skillTestSubscribers <> tokenSubscribers]
            <> [When (passed (SkillTestInitiatorTarget skillTestTarget))]
            <> [After $ passed target | target <- skillTestSubscribers <> tokenSubscribers]
            <> [After $ passed (SkillTestInitiatorTarget skillTestTarget)]
        FailedBy _ n -> do
          hauntedAbilities <- case (skillTestTarget, skillTestAction) of
            (LocationTarget lid, Just Action.Investigate) -> select $ HauntedAbility <> AbilityOnLocation (LocationWithId lid)
            _ -> pure []

          investigatorsToResolveFailure <-
            (`notNullOr` [skillTestInvestigator])
              <$> select (InvestigatorWithModifier ResolvesFailedEffects)

          let needsChoice = skillTestResolveFailureInvestigator `notElem` investigatorsToResolveFailure
          let
            handleChoice resolver player =
              let failed target = FailedSkillTest resolver skillTestAction skillTestSource target skillTestType n
               in [When (failed (Initiator skillTestTarget))]
                    <> [When (failed target) | target <- skillTestSubscribers <> tokenSubscribers]
                    <> [ chooseOneAtATime player [AbilityLabel resolver ab [] [] [] | ab <- hauntedAbilities]
                       | notNull hauntedAbilities
                       ]
                    <> [After $ failed target | target <- skillTestSubscribers <> tokenSubscribers]
                    <> [After $ failed (SkillTestInitiatorTarget skillTestTarget)]

          if needsChoice
            then do
              resolversWithPlayers <- traverse (traverseToSnd getPlayer) investigatorsToResolveFailure
              lead <- getLeadPlayer

              push
                $ chooseOrRunOne
                  lead
                  [ targetLabel resolver
                    $ SetSkillTestResolveFailureInvestigator resolver
                    : handleChoice resolver player
                  | (resolver, player) <- resolversWithPlayers
                  ]
            else do
              player <- getPlayer skillTestResolveFailureInvestigator
              pushAll $ handleChoice skillTestResolveFailureInvestigator player
        Unrun -> pure ()
      pure s
    AddSubscriber t -> pure $ s & subscribersL <>~ [t]
    RerunSkillTest -> case skillTestResult of
      FailedBy Automatic _ -> pure s
      _ -> do
        player <- getPlayer skillTestInvestigator
        withQueue_ $ filter $ \case
          Will FailedSkillTest {} -> False
          Will PassedSkillTest {} -> False
          CheckWindows [Window Timing.When (Window.WouldFailSkillTest _) _] ->
            False
          CheckWindows [Window Timing.When (Window.WouldPassSkillTest _) _] ->
            False
          Do (CheckWindows [Window Timing.When (Window.WouldFailSkillTest _) _]) ->
            False
          Do (CheckWindows [Window Timing.When (Window.WouldPassSkillTest _) _]) ->
            False
          Ask player' (ChooseOne [SkillTestApplyResultsButton])
            | player == player' -> False
          _ -> True
        push $ RunSkillTest skillTestInvestigator
        pure s
    RecalculateSkillTestResults -> runMessage (RecalculateSkillTestResultsCanChangeAutomatic False) s
    RecalculateSkillTestResultsCanChangeAutomatic canChange -> do
      let
        isAutomatic =
          if canChange
            then NonAutomatic
            else case skillTestResult of
              FailedBy fType _ -> fType
              SucceededBy fType _ -> fType
              _ -> NonAutomatic

      results <- case skillTestResult of
        FailedBy Automatic _ | not canChange -> autoFailSkillTestResultsData s
        _ -> calculateSkillTestResultsData s

      push $ SkillTestResults results
      modifiedSkillValue' <- totalModifiedSkillValue s
      let
        result =
          if skillTestResultsSuccess results
            then
              let
                succeededBy =
                  if isAutomatic == Automatic
                    then modifiedSkillValue'
                    else (modifiedSkillValue' - skillTestResultsDifficulty results)
               in
                SucceededBy isAutomatic succeededBy
            else
              let
                failedBy =
                  if isAutomatic == Automatic
                    then skillTestResultsDifficulty results
                    else skillTestResultsDifficulty results - modifiedSkillValue'
               in
                FailedBy isAutomatic failedBy
      pure $ s & resultL .~ result
    RunSkillTest _ -> do
      results <- calculateSkillTestResultsData s
      push $ SkillTestResults results
      -- TODO: We should be able to get all of this from the results data, but
      -- there is a discrepancy between totaledTokenValues and the info stored
      -- in the result data, this may be incorrect, need to investigate
      modifiedSkillValue' <- totalModifiedSkillValue s
      let
        result =
          if skillTestResultsSuccess results
            then SucceededBy NonAutomatic (modifiedSkillValue' - skillTestResultsDifficulty results)
            else FailedBy NonAutomatic (skillTestResultsDifficulty results - modifiedSkillValue')

      pure $ s & resultL .~ result & stepL .~ DetermineSuccessOrFailureOfSkillTestStep
    ChangeSkillTestType newSkillTestType newSkillTestBaseValue -> do
      let iconValues = iconValuesForSkillTestType newSkillTestType
      pure
        $ s
        & (typeL .~ newSkillTestType)
        & (baseValueL .~ newSkillTestBaseValue)
        & (iconValuesL .~ iconValues)
    RemoveAllChaosTokens face -> do
      pure $ s & setAsideChaosTokensL %~ filter ((/= face) . chaosTokenFace)
    SetSkillTestResolveFailureInvestigator iid -> do
      pure $ s & resolveFailureInvestigatorL .~ iid
    _ -> pure s
