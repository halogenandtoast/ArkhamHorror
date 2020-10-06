{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.SkillTest
  ( SkillTest(..)
  , SkillTestResult(..)
  , TokenResponse(..)
  , initSkillTest
  )
where

import Arkham.Json
import Arkham.Types.Action (Action)
import Arkham.Types.Card
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard (playerCardAttrs)
import Arkham.Types.Card.PlayerCard.Attrs (pcSkills)
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.SkillTestResult
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TokenResponse
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List as L
import Lens.Micro
import System.Environment

data SkillTest a = SkillTest
  { skillTestInvestigator    :: InvestigatorId
  , skillTestSkillType :: SkillType
  , skillTestDifficulty      :: Int
  , skillTestOnSuccess       :: [a]
  , skillTestOnFailure       :: [a]
  , skillTestOnTokenResponses :: [TokenResponse a]
  , skillTestSetAsideTokens  :: [Token]
  , skillTestRevealedTokens  :: [Token] -- tokens may change from physical representation
  , skillTestValueModifier :: Int
  , skillTestResult :: SkillTestResult
  , skillTestSkillValue :: Int
  , skillTestModifiers :: HashMap Source [Modifier]
  , skillTestTempModifiers :: [Modifier]
  , skillTestCommittedCards :: HashMap CardId (InvestigatorId, Card)
  , skillTestSource :: Source
  , skillTestAction :: Maybe Action
  , skillTestSubscribers :: [Target]
  }
  deriving stock (Show, Generic)

instance ToJSON a => ToJSON (SkillTest a) where
  toJSON = genericToJSON $ aesonOptions $ Just "skillTest"
  toEncoding = genericToEncoding $ aesonOptions $ Just "skillTest"

instance FromJSON a => FromJSON (SkillTest a) where
  parseJSON = genericParseJSON $ aesonOptions $ Just "skillTest"

instance HasSet CommittedCardId InvestigatorId (SkillTest a) where
  getSet iid =
    HashSet.map CommittedCardId
      . keysSet
      . filterMap ((== iid) . fst)
      . skillTestCommittedCards

instance HasSet CommittedCardCode () (SkillTest a) where
  getSet _ =
    setFromList
      . map (CommittedCardCode . getCardCode . snd)
      . toList
      . skillTestCommittedCards

initSkillTest
  :: InvestigatorId
  -> Source
  -> Maybe Action
  -> SkillType
  -> Int
  -> Int
  -> [Message]
  -> [Message]
  -> [Modifier]
  -> [TokenResponse Message]
  -> SkillTest Message
initSkillTest iid source maction skillType' skillValue' difficulty' onSuccess' onFailure' modifiers' tokenResponses'
  = SkillTest
    { skillTestInvestigator = iid
    , skillTestSkillType = skillType'
    , skillTestDifficulty = difficulty'
    , skillTestOnSuccess = onSuccess'
    , skillTestOnFailure = onFailure'
    , skillTestOnTokenResponses = tokenResponses'
    , skillTestSetAsideTokens = mempty
    , skillTestRevealedTokens = mempty
    , skillTestValueModifier = 0
    , skillTestResult = Unrun
    , skillTestSkillValue = skillValue'
    , skillTestTempModifiers = modifiers'
    , skillTestModifiers = mempty
    , skillTestCommittedCards = mempty
    , skillTestSource = source
    , skillTestAction = maction
    , skillTestSubscribers = [SkillTestInitiatorTarget, InvestigatorTarget iid]
    }

modifiers :: Lens' (SkillTest a) (HashMap Source [Modifier])
modifiers = lens skillTestModifiers $ \m x -> m { skillTestModifiers = x }

subscribers :: Lens' (SkillTest a) [Target]
subscribers =
  lens skillTestSubscribers $ \m x -> m { skillTestSubscribers = x }

-- skillValue :: Lens' (SkillTest a) Int
-- skillValue = lens skillTestSkillValue $ \m x -> m { skillTestSkillValue = x }

valueModifier :: Lens' (SkillTest a) Int
valueModifier =
  lens skillTestValueModifier $ \m x -> m { skillTestValueModifier = x }

setAsideTokens :: Lens' (SkillTest a) [Token]
setAsideTokens =
  lens skillTestSetAsideTokens $ \m x -> m { skillTestSetAsideTokens = x }

revealedTokens :: Lens' (SkillTest a) [Token]
revealedTokens =
  lens skillTestRevealedTokens $ \m x -> m { skillTestRevealedTokens = x }

committedCards :: Lens' (SkillTest a) (HashMap CardId (InvestigatorId, Card))
committedCards =
  lens skillTestCommittedCards $ \m x -> m { skillTestCommittedCards = x }

result :: Lens' (SkillTest a) SkillTestResult
result = lens skillTestResult $ \m x -> m { skillTestResult = x }

onTokenResponses :: Lens' (SkillTest a) [TokenResponse a]
onTokenResponses =
  lens skillTestOnTokenResponses $ \m x -> m { skillTestOnTokenResponses = x }

applicableModifiers :: SkillTest a -> [Modifier]
applicableModifiers SkillTest {..} = mapMaybe
  applicableModifier
  ((concat . toList $ skillTestModifiers) <> skillTestTempModifiers)
 where
  applicableModifier (ModifierIfSucceededBy n m) = case skillTestResult of
    SucceededBy x | x >= n -> Just m
    _ -> Nothing
  applicableModifier m = Just m

skillIconCount :: SkillTest a -> Int
skillIconCount SkillTest {..} = length . filter matches $ concatMap
  (iconsForCard . snd)
  (toList skillTestCommittedCards)
 where
  iconsForCard (PlayerCard pc) = pcSkills (playerCardAttrs pc)
  iconsForCard _ = []
  matches SkillWild = True
  matches s = s == skillTestSkillType

skillValueModifiers :: SkillTest a -> Int
skillValueModifiers SkillTest {..} = foldr
  applyModifier
  0
  ((concat . toList $ skillTestModifiers) <> skillTestTempModifiers)
 where
  applyModifier (AnySkillValue m) n = max 0 (n + m)
  applyModifier (SkillModifier stype m) n | skillTestSkillType == stype =
    max 0 (n + m)
  applyModifier _ n = n

modifiedTokenValue :: Int -> SkillTest a -> Int
modifiedTokenValue baseValue SkillTest {..} = foldr
  applyModifier
  baseValue
  ((concat . toList $ skillTestModifiers) <> skillTestTempModifiers)
 where
  applyModifier DoubleNegativeModifiersOnTokens n =
    if baseValue < 0 then n + baseValue else n
  applyModifier _ n = n

type SkillTestRunner env = (HasQueue env, HasCard InvestigatorId env)

instance (SkillTestRunner env) => RunMessage env (SkillTest Message) where
  runMessage msg s@SkillTest {..} = case msg of
    TriggerSkillTest iid -> do
      s <$ unshiftMessage (RequestTokens SkillTestSource iid 1 SetAside)
    DrawAnotherToken iid valueModifier' -> do
      unshiftMessage (RequestTokens SkillTestSource iid 1 SetAside)
      pure $ s & valueModifier +~ valueModifier'
    RequestedTokens SkillTestSource iid tokens -> do
      unshiftMessage (RevealSkillTestTokens iid)
      for_ tokens $ \token -> unshiftMessages
        [ When (RevealToken SkillTestSource iid token)
        , RevealToken SkillTestSource iid token
        ]
      pure $ s & (setAsideTokens %~ (tokens <>))
    RevealToken SkillTestSource _iid token -> do
      pure $ s & revealedTokens %~ (token :)
    RevealSkillTestTokens iid -> do
      onTokenResponses' <-
        (catMaybes <$>) . for skillTestOnTokenResponses $ \case
          OnAnyToken tokens' messages
            | not (null $ skillTestRevealedTokens `L.intersect` tokens')
            -> Nothing <$ unshiftMessages messages
          response -> pure (Just response)
      unshiftMessages
        [ ResolveToken token iid | token <- skillTestRevealedTokens ]
      pure
        $ s
        & (onTokenResponses .~ onTokenResponses')
        & (subscribers
          %~ (<> [ TokenTarget token' | token' <- skillTestRevealedTokens ])
          )
    AddSkillTestSubscriber target -> pure $ s & subscribers %~ (target :)
    SetAsideToken token -> pure $ s & (setAsideTokens %~ (token :))
    PassSkillTest -> do
      unshiftMessages
        [ Ask skillTestInvestigator $ ChooseOne [SkillTestApplyResults]
        , SkillTestEnds
        ]
      pure $ s & result .~ SucceededBy 0
    FailSkillTest -> do
      unshiftMessages
        [ Ask skillTestInvestigator $ ChooseOne [SkillTestApplyResults]
        , SkillTestEnds
        ]
      pure $ s & result .~ FailedBy skillTestDifficulty
    StartSkillTest _ -> s <$ unshiftMessages
      (HashMap.foldMapWithKey
          (\k (i, _) -> [CommitCard i k])
          skillTestCommittedCards
      <> [TriggerSkillTest skillTestInvestigator]
      )
    InvestigatorCommittedSkill _ skillId -> do
      pure $ s & subscribers %~ (SkillTarget skillId :)
    SkillTestCommitCard iid cardId -> do
      card <- asks (getCard iid cardId)
      pure $ s & committedCards %~ insertMap cardId (iid, card)
    SkillTestUncommitCard _ cardId ->
      pure $ s & committedCards %~ deleteMap cardId
    AddModifiers SkillTestTarget source modifiers' ->
      pure $ s & modifiers %~ insertWith (<>) source modifiers'
    ReturnSkillTestRevealedTokens -> do
      -- Rex's Curse timing keeps effects on stack so we do
      -- not want to remove them as subscribers from the stack
      unshiftMessage $ ResetTokens SkillTestSource
      pure $ s & setAsideTokens .~ mempty
    SkillTestEnds -> s <$ unshiftMessages
      [ RemoveAllModifiersOnTargetFrom
        (InvestigatorTarget skillTestInvestigator)
        SkillTestSource
      , ResetTokens SkillTestSource
      ]
    SkillTestResults -> do
      unshiftMessage
        (Ask skillTestInvestigator $ ChooseOne [SkillTestApplyResults])
      case skillTestResult of
        SucceededBy n -> unshiftMessages
          [ Will
              (PassedSkillTest
                skillTestInvestigator
                skillTestAction
                skillTestSource
                target
                n
              )
          | target <- skillTestSubscribers
          ]
        FailedBy n -> unshiftMessages
          [ Will
              (FailedSkillTest
                skillTestInvestigator
                skillTestAction
                skillTestSource
                target
                n
              )
          | target <- skillTestSubscribers
          ]
        Unrun -> pure ()
      pure s
    AddModifiers AfterSkillTestTarget source modifiers' ->
      pure $ s & modifiers %~ insertWith (<>) source modifiers'
    SkillTestApplyResultsAfter -> do -- ST.7 -- apply results
      unshiftMessage SkillTestEnds -- -> ST.8 -- Skill test ends

      case skillTestResult of
        SucceededBy n ->
          unshiftMessages
            $ skillTestOnSuccess
            <> [ After
                   (PassedSkillTest
                     skillTestInvestigator
                     skillTestAction
                     skillTestSource
                     target
                     n
                   )
               | target <- skillTestSubscribers
               ]
        FailedBy n ->
          unshiftMessages
            $ skillTestOnFailure
            <> [ After
                   (FailedSkillTest
                     skillTestInvestigator
                     skillTestAction
                     skillTestSource
                     target
                     n
                   )
               | target <- skillTestSubscribers
               ]
        Unrun -> pure ()

      s <$ unshiftMessage
        (AddModifiers
          (InvestigatorTarget skillTestInvestigator)
          SkillTestSource
          (applicableModifiers s)
        )
    SkillTestApplyResults -> do -- ST.7 Apply Results
      unshiftMessage SkillTestApplyResultsAfter
      s <$ case skillTestResult of
        SucceededBy n -> unshiftMessages
          [ PassedSkillTest
              skillTestInvestigator
              skillTestAction
              skillTestSource
              target
              n
          | target <- skillTestSubscribers
          ]
        FailedBy n -> unshiftMessages
          [ FailedSkillTest
              skillTestInvestigator
              skillTestAction
              skillTestSource
              target
              n
          | target <- skillTestSubscribers
          ]
        Unrun -> pure ()
    RunSkillTest _ (TokenValue _ tokenValue) -> do
      let
        totaledTokenValues =
          modifiedTokenValue tokenValue s + skillTestValueModifier
        modifiedSkillValue' =
          skillTestSkillValue
            + totaledTokenValues
            + skillIconCount s
            + skillValueModifiers s
      unshiftMessage SkillTestResults
      liftIO $ whenM
        (isJust <$> lookupEnv "DEBUG")
        (putStrLn
        . pack
        $ "skill value: "
        <> show skillTestSkillValue
        <> "\n+ totaled token values: "
        <> show totaledTokenValues
        <> "\n+ skill icon count: "
        <> show (skillIconCount s)
        <> "\n+ skill value modifiers: "
        <> show (skillValueModifiers s)
        <> "\n-------------------------"
        <> "\n= Modified skill value: "
        <> show modifiedSkillValue'
        <> "\nDifficulty: "
        <> show skillTestDifficulty
        )
      if modifiedSkillValue' >= skillTestDifficulty
        then
          pure
          $ s
          & result
          .~ SucceededBy (modifiedSkillValue' - skillTestDifficulty)
          & valueModifier
          .~ totaledTokenValues
        else
          pure
          $ s
          & result
          .~ FailedBy (skillTestDifficulty - modifiedSkillValue')
          & valueModifier
          .~ totaledTokenValues
    _ -> pure s
