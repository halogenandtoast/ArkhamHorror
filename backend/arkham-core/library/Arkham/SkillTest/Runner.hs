{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.SkillTest.Runner
  ( module X
  , skillIconCount
  ) where

import Arkham.Prelude

import Arkham.SkillTest as X

import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.Classes
import Arkham.Game.Helpers hiding ( matches )
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator
import Arkham.Message
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
import Data.HashMap.Strict qualified as HashMap

skillIconCount :: SkillTest -> GameT Int
skillIconCount SkillTest {..} = do
  investigatorModifiers <- getModifiers
    (InvestigatorTarget skillTestInvestigator)
  if SkillCannotBeIncreased skillTestSkillType `elem` investigatorModifiers
    then pure 0
    else length . filter matches <$> concatMapM
      (iconsForCard . snd)
      (toList skillTestCommittedCards)
 where
  iconsForCard c@(PlayerCard MkPlayerCard {..}) = do
    modifiers' <- getModifiers (CardIdTarget pcId)
    pure $ foldr
      applyAfterSkillModifiers
      (foldr applySkillModifiers (cdSkills $ toCardDef c) modifiers')
      modifiers'
  iconsForCard _ = pure []
  matches WildIcon = True
  matches (SkillIcon s) = s == skillTestSkillType
  applySkillModifiers (AddSkillIcons xs) ys = xs <> ys
  applySkillModifiers (RemoveSkillIcons xs) ys = ys \\ xs
  applySkillModifiers _ ys = ys
  applyAfterSkillModifiers DoubleSkillIcons ys = ys <> ys
  applyAfterSkillModifiers _ ys = ys

getModifiedSkillTestDifficulty :: SkillTest -> GameT Int
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
getModifiedTokenValue :: SkillTest -> Token -> GameT Int
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
    TriggerSkillTest iid -> do
      modifiers' <- getModifiers (InvestigatorTarget iid)
      if DoNotDrawChaosTokensForSkillChecks `elem` modifiers'
        then do
          let
            tokensTreatedAsRevealed = flip mapMaybe modifiers' $ \case
              TreatRevealedTokenAs t -> Just t
              _ -> Nothing
          if null tokensTreatedAsRevealed
            then s <$ push (RunSkillTest iid)
            else do
              pushAll [RevealSkillTestTokens iid, RunSkillTest iid]
              for_ tokensTreatedAsRevealed $ \tokenFace -> do
                t <- getRandom
                pushAll
                  $ resolve (RevealToken (toSource s) iid (Token t tokenFace))
              pure s
        else if SkillTestAutomaticallySucceeds `elem` modifiers'
          then s <$ push PassSkillTest
          else do
            let
              applyRevealStategyModifier _ (ChangeRevealStrategy n) = n
              applyRevealStategyModifier n _ = n
              revealStrategy =
                foldl' applyRevealStategyModifier (Reveal 1) modifiers'
            s <$ pushAll
              [ RequestTokens (toSource s) (Just iid) revealStrategy SetAside
              , RunSkillTest iid
              ]
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
          %~ (<> [ TokenTarget token' | token' <- skillTestRevealedTokens ])
          )
    PassSkillTest -> do
      stats <- modifiedStatsOf skillTestAction skillTestInvestigator
      iconCount <- skillIconCount s
      let
        currentSkillValue = statsSkillValue stats skillTestSkillType
        modifiedSkillValue' =
          max 0 (currentSkillValue + skillTestValueModifier + iconCount)
      pushAll
        [ chooseOne skillTestInvestigator [SkillTestApplyResultsButton]
        , SkillTestEnds skillTestInvestigator skillTestSource
        ]
      pure $ s & resultL .~ SucceededBy True modifiedSkillValue'
    FailSkillTest -> do
      difficulty <- getModifiedSkillTestDifficulty s
      pushAll
        $ [ Will
              (FailedSkillTest
                skillTestInvestigator
                skillTestAction
                skillTestSource
                target
                skillTestSkillType
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
               skillTestSkillType
               difficulty
             )
           , chooseOne skillTestInvestigator [SkillTestApplyResultsButton]
           , SkillTestEnds skillTestInvestigator skillTestSource
           ]
      pure $ s & resultL .~ FailedBy True difficulty
    StartSkillTest _ -> do
      windowMsg <- checkWindows [Window Timing.When Window.FastPlayerWindow]
      s <$ pushAll
        (HashMap.foldMapWithKey
            (\k (i, _) -> [CommitCard i k])
            skillTestCommittedCards
        <> [windowMsg, TriggerSkillTest skillTestInvestigator]
        )
    InvestigatorCommittedSkill _ skillId ->
      pure $ s & subscribersL %~ (SkillTarget skillId :)
    PutCardIntoPlay _ card _ _ -> do
      pure $ s & committedCardsL %~ deleteMap (toCardId card)
    SkillTestCommitCard iid card -> do
      pure $ s & committedCardsL %~ insertMap (toCardId card) (iid, card)
    SkillTestUncommitCard _ card ->
      pure $ s & committedCardsL %~ deleteMap (toCardId card)
    ReturnSkillTestRevealedTokens -> do
      -- Rex's Curse timing keeps effects on stack so we do
      -- not want to remove them as subscribers from the stack
      push $ ResetTokens (toSource s)
      pure
        $ s
        & (setAsideTokensL .~ mempty)
        & (revealedTokensL .~ mempty)
        & (resolvedTokensL .~ mempty)
    SkillTestEnds _ _ -> do
      -- Skill Cards are in the environment and will be discarded normally
      -- However, all other cards need to be discarded here.
      let
        discards = mapMaybe
          (\case
            (iid, PlayerCard pc) ->
              (iid, pc) <$ guard (cdCardType (toCardDef pc) /= SkillType)
            (_, EncounterCard _) -> Nothing
            (_, VengeanceCard _) -> Nothing
          )
          (s ^. committedCardsL . to toList)
        skillResultValue = case skillTestResult of
          Unrun -> error "wat, skill test has to run"
          SucceededBy _ n -> n
          FailedBy _ n -> (-n)

      skillTestEndsWindows <- windows [Window.SkillTestEnded s]
      s <$ pushAll
        (ResetTokens (toSource s)
        : map (uncurry AddToDiscard) discards
        <> skillTestEndsWindows
        <> [ AfterSkillTestEnds
               skillTestSource
               skillTestTarget
               skillResultValue
           ]
        )
    SkillTestResults{} -> do
      modifiers' <- getModifiers (toTarget s)
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
                 skillTestSkillType
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
                   skillTestSkillType
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
                 skillTestSkillType
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
                   skillTestSkillType
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
                 skillTestSkillType
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
                   skillTestSkillType
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
                 skillTestSkillType
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
                   skillTestSkillType
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
                    skillTestSkillType
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
                     skillTestSkillType
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
                      skillTestSkillType
                      n
                  | target <- skillTestSubscribers
                  ]
                 <> [ PassedSkillTest
                        skillTestInvestigator
                        skillTestAction
                        skillTestSource
                        (SkillTestInitiatorTarget skillTestTarget)
                        skillTestSkillType
                        n
                    ]
                 )
               )
        FailedBy _ n -> pushAll
          ([ When
               (FailedSkillTest
                 skillTestInvestigator
                 skillTestAction
                 skillTestSource
                 (SkillTestInitiatorTarget skillTestTarget)
                 skillTestSkillType
                 n
               )
           ]
          <> [ When
                 (FailedSkillTest
                   skillTestInvestigator
                   skillTestAction
                   skillTestSource
                   target
                   skillTestSkillType
                   n
                 )
             | target <- skillTestSubscribers
             ]
          <> [ FailedSkillTest
                 skillTestInvestigator
                 skillTestAction
                 skillTestSource
                 target
                 skillTestSkillType
                 n
             | target <- skillTestSubscribers
             ]
          <> [ FailedSkillTest
                 skillTestInvestigator
                 skillTestAction
                 skillTestSource
                 (SkillTestInitiatorTarget skillTestTarget)
                 skillTestSkillType
                 n
             ]
          )
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
      modifiers' <- getModifiers SkillTestTarget
      stats <- modifiedStatsOf skillTestAction skillTestInvestigator
      modifiedSkillTestDifficulty <- getModifiedSkillTestDifficulty s
      iconCount <- if CancelSkills `elem` modifiers'
        then pure 0
        else skillIconCount s
      let
        currentSkillValue = statsSkillValue stats skillTestSkillType
        addResultModifier n (SkillTestResultValueModifier m) = n + m
        addResultModifier n _ = n
        resultValueModifiers = foldl' addResultModifier 0 modifiers'
      push $ SkillTestResults $ SkillTestResultsData
        currentSkillValue
        iconCount
        skillTestValueModifier
        modifiedSkillTestDifficulty
        (resultValueModifiers <$ guard (resultValueModifiers /= 0))
      pure s
    RunSkillTest _ -> do
      modifiers' <- getModifiers SkillTestTarget
      tokenValues <- sum <$> for
        (skillTestRevealedTokens <> skillTestResolvedTokens)
        (getModifiedTokenValue s)
      stats <- modifiedStatsOf skillTestAction skillTestInvestigator
      modifiedSkillTestDifficulty <- getModifiedSkillTestDifficulty s
      iconCount <- if CancelSkills `elem` modifiers'
        then pure 0
        else skillIconCount s
      let
        currentSkillValue = statsSkillValue stats skillTestSkillType
        totaledTokenValues = tokenValues + skillTestValueModifier
        modifiedSkillValue' =
          max 0 (currentSkillValue + totaledTokenValues + iconCount)
        addResultModifier n (SkillTestResultValueModifier m) = n + m
        addResultModifier n _ = n
        resultValueModifiers = foldl' addResultModifier 0 modifiers'
      push $ SkillTestResults $ SkillTestResultsData
        currentSkillValue
        iconCount
        totaledTokenValues
        modifiedSkillTestDifficulty
        (resultValueModifiers <$ guard (resultValueModifiers /= 0))
      if modifiedSkillValue' >= modifiedSkillTestDifficulty
        then
          pure
          $ s
          & (resultL .~ SucceededBy
              False
              (modifiedSkillValue' - modifiedSkillTestDifficulty)
            )
          & (valueModifierL .~ totaledTokenValues)
        else
          pure
          $ s
          & (resultL .~ FailedBy
              False
              (modifiedSkillTestDifficulty - modifiedSkillValue')
            )
          & (valueModifierL .~ totaledTokenValues)
    _ -> pure s
