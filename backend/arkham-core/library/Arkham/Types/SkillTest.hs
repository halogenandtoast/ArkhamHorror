{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.SkillTest
  ( SkillTest(..)
  , DrawStrategy(..)
  , ResolveStrategy(..)
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
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillTestResult
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TokenResponse
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Lens.Micro

data DrawStrategy
  = DrawOne
  | DrawX Int
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ResolveStrategy
  = ResolveAll
  | ResolveOne
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SkillTest a = SkillTest
  { skillTestInvestigator    :: InvestigatorId
  , skillTestSkillType :: SkillType
  , skillTestDifficulty      :: Int
  , skillTestOnSuccess       :: [a]
  , skillTestOnFailure       :: [a]
  , skillTestOnTokenResponses :: [TokenResponse a]
  , skillTestDrawStrategy    :: DrawStrategy
  , skillTestResolveStrategy :: ResolveStrategy
  , skillTestSetAsideTokens  :: [Token]
  , skillTestValueModifier :: Int
  , skillTestResult :: SkillTestResult
  , skillTestModifiers :: HashMap Source [Modifier]
  , skillTestTempModifiers :: [Modifier]
  , skillTestCommittedCards :: HashMap CardId (InvestigatorId, Card)
  , skillTestSource :: Source
  , skillTestAction :: Maybe Action
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
      . HashMap.keysSet
      . HashMap.filter ((== iid) . fst)
      . skillTestCommittedCards

instance HasSet CommittedCardCode () (SkillTest a) where
  getSet _ =
    HashSet.fromList
      . map (CommittedCardCode . getCardCode . snd)
      . HashMap.elems
      . skillTestCommittedCards

initSkillTest
  :: InvestigatorId
  -> Source
  -> Maybe Action
  -> SkillType
  -> Int
  -> [Message]
  -> [Message]
  -> [Modifier]
  -> [TokenResponse Message]
  -> SkillTest Message
initSkillTest iid source maction skillType' difficulty' onSuccess' onFailure' modifiers' tokenResponses'
  = SkillTest
    { skillTestInvestigator = iid
    , skillTestSkillType = skillType'
    , skillTestDifficulty = difficulty'
    , skillTestOnSuccess = onSuccess'
    , skillTestOnFailure = onFailure'
    , skillTestOnTokenResponses = tokenResponses'
    , skillTestDrawStrategy = DrawOne
    , skillTestResolveStrategy = ResolveAll
    , skillTestSetAsideTokens = mempty
    , skillTestValueModifier = 0
    , skillTestResult = Unrun
    , skillTestTempModifiers = modifiers'
    , skillTestModifiers = mempty
    , skillTestCommittedCards = mempty
    , skillTestSource = source
    , skillTestAction = maction
    }

modifiers :: Lens' (SkillTest a) (HashMap Source [Modifier])
modifiers = lens skillTestModifiers $ \m x -> m { skillTestModifiers = x }

valueModifier :: Lens' (SkillTest a) Int
valueModifier =
  lens skillTestValueModifier $ \m x -> m { skillTestValueModifier = x }

setAsideTokens :: Lens' (SkillTest a) [Token]
setAsideTokens =
  lens skillTestSetAsideTokens $ \m x -> m { skillTestSetAsideTokens = x }

committedCards :: Lens' (SkillTest a) (HashMap CardId (InvestigatorId, Card))
committedCards =
  lens skillTestCommittedCards $ \m x -> m { skillTestCommittedCards = x }

result :: Lens' (SkillTest a) SkillTestResult
result = lens skillTestResult $ \m x -> m { skillTestResult = x }

onFailure :: Lens' (SkillTest a) [a]
onFailure = lens skillTestOnFailure $ \m x -> m { skillTestOnFailure = x }

onSuccess :: Lens' (SkillTest a) [a]
onSuccess = lens skillTestOnSuccess $ \m x -> m { skillTestOnSuccess = x }

onTokenResponses :: Lens' (SkillTest a) [TokenResponse a]
onTokenResponses =
  lens skillTestOnTokenResponses $ \m x -> m { skillTestOnTokenResponses = x }

applicableModifiers :: SkillTest a -> [Modifier]
applicableModifiers SkillTest {..} = mapMaybe
  applicableModifier
  ((concat . HashMap.elems $ skillTestModifiers) <> skillTestTempModifiers)
 where
  applicableModifier (ModifierIfSucceededBy n m) = case skillTestResult of
    SucceededBy x | x >= n -> Just m
    _ -> Nothing
  applicableModifier m = Just m

skillIconCount :: SkillTest a -> Int
skillIconCount SkillTest {..} = length . filter matches $ concatMap
  (iconsForCard . snd)
  (HashMap.elems skillTestCommittedCards)
 where
  iconsForCard (PlayerCard MkPlayerCard {..}) = pcSkills
  iconsForCard _ = []
  matches SkillWild = True
  matches s = s == skillTestSkillType

skillValueModifiers :: SkillTest a -> Int
skillValueModifiers SkillTest {..} = foldr
  applyModifier
  0
  ((concat . HashMap.elems $ skillTestModifiers) <> skillTestTempModifiers)
 where
  applyModifier (AnySkillValue m) n = max 0 (n + m)
  applyModifier _ n = n

modifiedTokenValue :: Int -> SkillTest a -> Int
modifiedTokenValue baseValue SkillTest {..} = foldr
  applyModifier
  baseValue
  ((concat . HashMap.elems $ skillTestModifiers) <> skillTestTempModifiers)
 where
  applyModifier DoubleNegativeModifiersOnTokens n =
    if baseValue < 0 then n + baseValue else n
  applyModifier _ n = n

type SkillTestRunner env = (HasQueue env, HasCard InvestigatorId env)

instance (SkillTestRunner env) => RunMessage env (SkillTest Message) where
  runMessage msg s@SkillTest {..} = case msg of
    AddOnFailure m -> pure $ s & onFailure %~ (m :)
    AddOnSuccess m -> pure $ s & onSuccess %~ (m :)
    HorrorPerPointOfFailure iid -> case skillTestResult of
      FailedBy n ->
        s <$ unshiftMessage (InvestigatorAssignDamage iid SkillTestSource 0 n)
      _ -> error "Should not be called when not failed"
    DamagePerPointOfFailure iid -> case skillTestResult of
      FailedBy n ->
        s <$ unshiftMessage (InvestigatorAssignDamage iid SkillTestSource n 0)
      _ -> error "Should not be called when not failed"
    DrawToken token -> do
      onTokenResponses' <-
        (catMaybes <$>) . for skillTestOnTokenResponses $ \case
          OnAnyToken tokens messages | token `elem` tokens ->
            Nothing <$ unshiftMessages messages
          response -> pure (Just response)
      pure
        $ s
        & (setAsideTokens %~ (token :))
        & (onTokenResponses .~ onTokenResponses')
    DrawAnotherToken _ _ _ valueModifier' ->
      pure $ s & valueModifier +~ valueModifier'
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
      <> [ InvestigatorStartSkillTest
             skillTestInvestigator
             skillTestAction
             skillTestSkillType
             ((concat . HashMap.elems $ skillTestModifiers)
             <> skillTestTempModifiers
             )
         ]
      )
    SkillTestCommitCard iid cardId -> do
      card <- asks (getCard iid cardId)
      pure $ s & committedCards %~ HashMap.insert cardId (iid, card)
    SkillTestUncommitCard _ cardId ->
      pure $ s & committedCards %~ HashMap.delete cardId
    AddModifiers SkillTestTarget source modifiers' ->
      pure $ s & modifiers %~ HashMap.insertWith (<>) source modifiers'
    SkillTestEnds -> s <$ unshiftMessages
      [ RemoveAllModifiersOnTargetFrom
        (InvestigatorTarget skillTestInvestigator)
        SkillTestSource
      , ReturnTokens skillTestSetAsideTokens
      ]
    SkillTestResults -> do
      unshiftMessage
        (Ask skillTestInvestigator $ ChooseOne [SkillTestApplyResults])
      case skillTestResult of
        SucceededBy n -> unshiftMessage
          (Will
            (PassedSkillTest
              skillTestInvestigator
              skillTestAction
              skillTestSource
              n
            )
          )
        FailedBy n -> unshiftMessage
          (Will
            (FailedSkillTest
              skillTestInvestigator
              skillTestAction
              skillTestSource
              n
            )
          )
        Unrun -> pure ()
      pure s
    AddModifiers AfterSkillTestTarget source modifiers' ->
      pure $ s & modifiers %~ HashMap.insertWith (<>) source modifiers'
    SkillTestApplyResultsAfter -> do
      unshiftMessage SkillTestEnds

      case skillTestResult of
        SucceededBy n ->
          unshiftMessages
            $ skillTestOnSuccess
            <> [ After
                   (PassedSkillTest
                     skillTestInvestigator
                     skillTestAction
                     skillTestSource
                     n
                   )
               ]
        FailedBy n ->
          unshiftMessages
            $ skillTestOnFailure
            <> [ After
                   (FailedSkillTest
                     skillTestInvestigator
                     skillTestAction
                     skillTestSource
                     n
                   )
               ]
        Unrun -> pure ()

      s <$ unshiftMessage
        (AddModifiers
          (InvestigatorTarget skillTestInvestigator)
          SkillTestSource
          (applicableModifiers s)
        )
    SkillTestApplyResults -> do
      -- TODO: the player can sequence the skill test results in whatever order they want
      unshiftMessage SkillTestApplyResultsAfter
      s <$ case skillTestResult of
        SucceededBy n -> unshiftMessage
          (PassedSkillTest
            skillTestInvestigator
            skillTestAction
            skillTestSource
            n
          )
        FailedBy n -> unshiftMessage
          (FailedSkillTest
            skillTestInvestigator
            skillTestAction
            skillTestSource
            n
          )
        Unrun -> pure ()
    NotifyOnFailure iid target -> do
      case skillTestResult of
        FailedBy n -> s <$ unshiftMessage (SkillTestDidFailBy iid target n)
        _ -> pure s
    NotifyOnSuccess iid target -> do
      case skillTestResult of
        SucceededBy n -> s <$ unshiftMessage (SkillTestDidPassBy iid target n)
        _ -> pure s
    RunSkillTest skillValue tokenValue -> do
      let
        totaledTokenValues =
          modifiedTokenValue tokenValue s + skillTestValueModifier
      let
        modifiedSkillValue' =
          skillValue
            + totaledTokenValues
            + skillIconCount s
            + skillValueModifiers s
      unshiftMessage SkillTestResults
      putStrLn
        . pack
        $ "skill value: "
        <> show skillValue
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
      if modifiedSkillValue' >= skillTestDifficulty
        then pure $ s & result .~ SucceededBy
          (modifiedSkillValue' - skillTestDifficulty)
        else pure $ s & result .~ FailedBy
          (skillTestDifficulty - modifiedSkillValue')
    _ -> pure s
