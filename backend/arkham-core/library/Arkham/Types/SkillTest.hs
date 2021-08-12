module Arkham.Types.SkillTest
  ( module Arkham.Types.SkillTest
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Action (Action)
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.SkillTestResult
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Token
import Arkham.Types.Window
import qualified Data.HashMap.Strict as HashMap
import Data.Semigroup

class HasSkillTest env where
  getSkillTest :: MonadReader env m => m (Maybe SkillTest)

getSkillTestTarget :: (MonadReader env m, HasSkillTest env) => m (Maybe Target)
getSkillTestTarget = fmap skillTestTarget <$> getSkillTest

getSkillTestSource :: (MonadReader env m, HasSkillTest env) => m (Maybe Source)
getSkillTestSource = fmap toSource <$> getSkillTest

data SkillTestResultsData = SkillTestResultsData
  { skillTestResultsSkillValue :: Int
  , skillTestResultsIconValue :: Int
  , skillTestResultsTokensValue :: Int
  , skillTestResultsDifficulty :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SkillTest = SkillTest
  { skillTestInvestigator :: InvestigatorId
  , skillTestSkillType :: SkillType
  , skillTestDifficulty :: Int
  , skillTestSetAsideTokens :: [Token]
  , skillTestRevealedTokens :: [Token] -- tokens may change from physical representation
  , skillTestResolvedTokens :: [Token]
  , skillTestValueModifier :: Int
  , skillTestResult :: SkillTestResult
  , skillTestCommittedCards :: HashMap CardId (InvestigatorId, Card)
  , skillTestSource :: Source
  , skillTestTarget :: Target
  , skillTestAction :: Maybe Action
  , skillTestSubscribers :: [Target]
  }
  deriving stock (Show, Eq, Generic)

subscribersL :: Lens' SkillTest [Target]
subscribersL =
  lens skillTestSubscribers $ \m x -> m { skillTestSubscribers = x }

setAsideTokensL :: Lens' SkillTest [Token]
setAsideTokensL =
  lens skillTestSetAsideTokens $ \m x -> m { skillTestSetAsideTokens = x }

resolvedTokensL :: Lens' SkillTest [Token]
resolvedTokensL =
  lens skillTestResolvedTokens $ \m x -> m { skillTestResolvedTokens = x }

revealedTokensL :: Lens' SkillTest [Token]
revealedTokensL =
  lens skillTestRevealedTokens $ \m x -> m { skillTestRevealedTokens = x }

committedCardsL :: Lens' SkillTest (HashMap CardId (InvestigatorId, Card))
committedCardsL =
  lens skillTestCommittedCards $ \m x -> m { skillTestCommittedCards = x }

resultL :: Lens' SkillTest SkillTestResult
resultL = lens skillTestResult $ \m x -> m { skillTestResult = x }

valueModifierL :: Lens' SkillTest Int
valueModifierL =
  lens skillTestValueModifier $ \m x -> m { skillTestValueModifier = x }

instance TargetEntity SkillTest where
  toTarget _ = SkillTestTarget
  isTarget _ SkillTestTarget = True
  isTarget _ _ = False

instance SourceEntity SkillTest where
  toSource SkillTest {..} = SkillTestSource
    skillTestInvestigator
    skillTestSkillType
    skillTestSource
    skillTestTarget
    skillTestAction
  isSource _ SkillTestSource{} = True
  isSource _ _ = False

instance ToJSON SkillTest where
  toJSON = genericToJSON $ aesonOptions $ Just "skillTest"
  toEncoding = genericToEncoding $ aesonOptions $ Just "skillTest"

instance FromJSON SkillTest where
  parseJSON = genericParseJSON $ aesonOptions $ Just "skillTest"

-- TODO: Cursed Swamp would apply to anyone trying to commit skill cards
instance HasModifiersFor env SkillTest

instance HasList CommittedCard env (InvestigatorId, SkillTest) where
  getList (iid, st) =
    pure
      . map (CommittedCard . snd)
      . filter ((== iid) . fst)
      . toList
      $ skillTestCommittedCards st

instance HasSet CommittedCardId env (InvestigatorId, SkillTest) where
  getSet (iid, st) =
    pure
      . mapSet CommittedCardId
      . keysSet
      . filterMap ((== iid) . fst)
      $ skillTestCommittedCards st

instance HasSet CommittedSkillId env (InvestigatorId, SkillTest) where
  getSet (iid, st) =
    pure
      . mapSet (CommittedSkillId . SkillId)
      . keysSet
      . filterMap
          (\(iid', card') -> iid' == iid && toCardType card' == SkillType)
      $ skillTestCommittedCards st

instance HasModifiersFor env () => HasList CommittedSkillIcon env (InvestigatorId, SkillTest) where
  getList (iid, st) = do
    let
      cards = toList . filterMap ((== iid) . fst) $ skillTestCommittedCards st
    concatMapM (fmap (map CommittedSkillIcon) . iconsForCard . snd) cards
   where
    iconsForCard c@(PlayerCard MkPlayerCard {..}) = do
      modifiers' <- getModifiers (toSource st) (CardIdTarget pcId)
      pure $ foldr applySkillModifiers (cdSkills $ toCardDef c) modifiers'
    iconsForCard _ = pure []
    applySkillModifiers (AddSkillIcons xs) ys = xs <> ys
    applySkillModifiers _ ys = ys

instance HasSet CommittedCardCode env SkillTest where
  getSet =
    pure
      . setFromList
      . map (CommittedCardCode . cdCardCode . toCardDef . snd)
      . toList
      . skillTestCommittedCards

initSkillTest
  :: InvestigatorId
  -> Source
  -> Target
  -> Maybe Action
  -> SkillType
  -> Int
  -> Int
  -> SkillTest
initSkillTest iid source target maction skillType' _skillValue' difficulty' =
  SkillTest
    { skillTestInvestigator = iid
    , skillTestSkillType = skillType'
    , skillTestDifficulty = difficulty'
    , skillTestSetAsideTokens = mempty
    , skillTestRevealedTokens = mempty
    , skillTestResolvedTokens = mempty
    , skillTestValueModifier = 0
    , skillTestResult = Unrun
    , skillTestCommittedCards = mempty
    , skillTestSource = source
    , skillTestTarget = target
    , skillTestAction = maction
    , skillTestSubscribers = [InvestigatorTarget iid]
    }

skillIconCount
  :: (MonadReader env m, HasModifiersFor env ()) => SkillTest -> m Int
skillIconCount st@SkillTest {..} = length . filter matches <$> concatMapM
  (iconsForCard . snd)
  (toList skillTestCommittedCards)
 where
  iconsForCard c@(PlayerCard MkPlayerCard {..}) = do
    modifiers' <- getModifiers (toSource st) (CardIdTarget pcId)
    pure $ foldr applySkillModifiers (cdSkills $ toCardDef c) modifiers'
  iconsForCard _ = pure []
  matches SkillWild = True
  matches s = s == skillTestSkillType
  applySkillModifiers (AddSkillIcons xs) ys = xs <> ys
  applySkillModifiers _ ys = ys

getModifiedSkillTestDifficulty
  :: (MonadReader env m, HasModifiersFor env ()) => SkillTest -> m Int
getModifiedSkillTestDifficulty s = do
  modifiers' <- getModifiers (toSource s) SkillTestTarget
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

type SkillTestRunner env
  = ( HasQueue env
    , HasCard env InvestigatorId
    , HasStats env (InvestigatorId, Maybe Action)
    , HasSkillTest env
    , HasModifiersFor env ()
    , HasTokenValue env ()
    , HasId LocationId env InvestigatorId
    , HasSet ConnectedLocationId env LocationId
    , HasSet InvestigatorId env ()
    )

-- per the FAQ the double negative modifier ceases to be active
-- when Sure Gamble is used so we overwrite both Negative and DoubleNegative
getModifiedTokenValue
  :: ( MonadReader env m
     , HasModifiersFor env ()
     , HasTokenValue env ()
     , MonadIO m
     )
  => SkillTest
  -> Token
  -> m Int
getModifiedTokenValue s t = do
  tokenModifiers' <- getModifiers (toSource s) (TokenTarget t)
  modifiedTokenFaces' <- getModifiedTokenFaces s [t]
  getSum . mconcat <$> for
    modifiedTokenFaces'
    (\tokenFace -> do
      baseTokenValue <- getTokenValue () (skillTestInvestigator s) tokenFace
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

instance SkillTestRunner env => RunMessage env SkillTest where
  runMessage msg s@SkillTest {..} = case msg of
    TriggerSkillTest iid -> do
      modifiers' <- getModifiers (toSource s) (InvestigatorTarget iid)
      if DoNotDrawChaosTokensForSkillChecks `elem` modifiers'
        then
          s
            <$ pushAll
                 [ RunSkillTestSourceNotification iid skillTestSource
                 , RunSkillTest iid
                 ]
        else s <$ pushAll
          [RequestTokens (toSource s) (Just iid) 1 SetAside, RunSkillTest iid]
    DrawAnotherToken iid -> do
      pushAll
        [RequestTokens (toSource s) (Just iid) 1 SetAside, RunSkillTest iid]
      pure $ s & (resolvedTokensL %~ (<> skillTestRevealedTokens))
    RequestedTokens (SkillTestSource siid skillType source target maction) (Just iid) tokenFaces
      -> do
        push (RevealSkillTestTokens iid)
        for_ tokenFaces $ \tokenFace -> do
          pushAll $ resolve
            (RevealToken
              (SkillTestSource siid skillType source target maction)
              iid
              tokenFace
            )
        pure $ s & (setAsideTokensL %~ (tokenFaces <>))
    RevealToken SkillTestSource{} iid token -> do
      push (CheckWindow iid [AfterRevealToken iid token])
      pure $ s & revealedTokensL %~ (token :)
    RevealSkillTestTokens iid -> do
      revealedTokenFaces <- flip
        concatMapM
        (skillTestRevealedTokens \\ skillTestResolvedTokens)
        \token -> do
          faces <- getModifiedTokenFaces s [token]
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
    AddSkillTestSubscriber target -> pure $ s & subscribersL %~ (target :)
    PassSkillTest -> do
      stats <- getStats (skillTestInvestigator, skillTestAction) (toSource s)
      iconCount <- skillIconCount s
      let
        currentSkillValue = statsSkillValue stats skillTestSkillType
        modifiedSkillValue' =
          max 0 (currentSkillValue + skillTestValueModifier + iconCount)
      pushAll
        [ chooseOne skillTestInvestigator [SkillTestApplyResults]
        , SkillTestEnds skillTestSource
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
           , chooseOne skillTestInvestigator [SkillTestApplyResults]
           , SkillTestEnds skillTestSource
           ]
      pure $ s & resultL .~ FailedBy True difficulty
    StartSkillTest _ -> do
      windowMsgs <- checkWindows [FastPlayerWindow]
      s <$ pushAll
        (HashMap.foldMapWithKey
            (\k (i, _) -> [CommitCard i k])
            skillTestCommittedCards
        <> windowMsgs
        <> [TriggerSkillTest skillTestInvestigator]
        )
    InvestigatorCommittedSkill _ skillId ->
      pure $ s & subscribersL %~ (SkillTarget skillId :)
    SkillTestCommitCard iid cardId -> do
      card <- getCard cardId iid
      pure $ s & committedCardsL %~ insertMap cardId (iid, card)
    SkillTestUncommitCard _ cardId ->
      pure $ s & committedCardsL %~ deleteMap cardId
    ReturnSkillTestRevealedTokens -> do
      -- Rex's Curse timing keeps effects on stack so we do
      -- not want to remove them as subscribers from the stack
      push $ ResetTokens (toSource s)
      pure
        $ s
        & (setAsideTokensL .~ mempty)
        & (revealedTokensL .~ mempty)
        & (resolvedTokensL .~ mempty)
    SkillTestEnds _ -> do
      -- Skill Cards are in the environment and will be discarded normally
      -- However, all other cards need to be discarded here.
      let
        discards = mapMaybe
          (\case
            (iid, PlayerCard pc) ->
              (iid, pc) <$ guard (cdCardType (toCardDef pc) /= SkillType)
            (_, EncounterCard _) -> Nothing
          )
          (s ^. committedCardsL . to toList)
        skillResultValue = case skillTestResult of
          Unrun -> error "wat, skill test has to run"
          SucceededBy _ n -> n
          FailedBy _ n -> (-n)
      s <$ pushAll
        (ResetTokens (toSource s)
        : map (uncurry AddToDiscard) discards
        <> [ AfterSkillTestEnds
               skillTestSource
               skillTestTarget
               skillResultValue
           ]
        )
    SkillTestResults{} -> do
      push (chooseOne skillTestInvestigator [SkillTestApplyResults])
      case skillTestResult of
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
    SkillTestApplyResultsAfter -> do -- ST.7 -- apply results
      push $ SkillTestEnds skillTestSource -- -> ST.8 -- Skill test ends

      case skillTestResult of
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
    SkillTestApplyResults -> do -- ST.7 Apply Results
      push SkillTestApplyResultsAfter
      modifiers' <- getModifiers (toSource s) (toTarget s)
      let successTimes = if DoubleSuccess `elem` modifiers' then 2 else 1
      s <$ case skillTestResult of
        SucceededBy _ n -> pushAll $ cycleN
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
    RerunSkillTest -> case skillTestResult of
      FailedBy True _ -> pure s
      _ -> do
        withQueue_ $ filter $ \case
          Will FailedSkillTest{} -> False
          Will PassedSkillTest{} -> False
          CheckWindow _ [Window Timing.When WouldFailSkillTest _] -> False
          Ask skillTestInvestigator' (ChooseOne [SkillTestApplyResults])
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
    RunSkillTest _ -> do
      modifiers' <- getModifiers (toSource s) SkillTestTarget
      tokenValues <- sum <$> for
        (skillTestRevealedTokens <> skillTestResolvedTokens)
        (getModifiedTokenValue s)
      stats <- getStats (skillTestInvestigator, skillTestAction) (toSource s)
      modifiedSkillTestDifficulty <- getModifiedSkillTestDifficulty s
      iconCount <- if CancelSkills `elem` modifiers'
        then pure 0
        else skillIconCount s
      let
        currentSkillValue = statsSkillValue stats skillTestSkillType
        totaledTokenValues = tokenValues + skillTestValueModifier
        modifiedSkillValue' =
          max 0 (currentSkillValue + totaledTokenValues + iconCount)
      push $ SkillTestResults
        currentSkillValue
        iconCount
        totaledTokenValues
        modifiedSkillTestDifficulty
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
