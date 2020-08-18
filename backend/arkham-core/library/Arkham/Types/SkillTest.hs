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
import Arkham.Types.Action (ActionType)
import Arkham.Types.Card
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
  , skillTestResult :: SkillTestResult
  , skillTestModifiers :: [Modifier]
  , skillTestCommittedCards :: HashMap CardId (InvestigatorId, Card)
  , skillTestSource :: Source
  , skillTestAction :: Maybe ActionType
  }
  deriving stock (Show, Generic)

instance ToJSON a => ToJSON (SkillTest a) where
  toJSON = genericToJSON $ aesonOptions $ Just "skillTest"
  toEncoding = genericToEncoding $ aesonOptions $ Just "skillTest"

instance FromJSON a => FromJSON (SkillTest a) where
  parseJSON = genericParseJSON $ aesonOptions $ Just "skillTest"

instance HasSet CommitedCardId InvestigatorId (SkillTest a) where
  getSet iid =
    HashSet.map CommitedCardId
      . HashMap.keysSet
      . HashMap.filter ((== iid) . fst)
      . skillTestCommittedCards

initSkillTest
  :: InvestigatorId
  -> Source
  -> Maybe ActionType
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
    , skillTestResult = Unrun
    , skillTestModifiers = modifiers'
    , skillTestCommittedCards = mempty
    , skillTestSource = source
    , skillTestAction = maction
    }

modifiers :: Lens' (SkillTest a) [Modifier]
modifiers = lens skillTestModifiers $ \m x -> m { skillTestModifiers = x }

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

skillIconCount :: SkillTest a -> Int
skillIconCount SkillTest {..} = length . filter matches $ concatMap
  (iconsForCard . snd)
  (HashMap.elems skillTestCommittedCards)
 where
  iconsForCard (PlayerCard MkPlayerCard {..}) = pcSkills
  iconsForCard _ = []
  matches SkillWild = True
  matches s = s == skillTestSkillType

modifiedTokenValue :: Int -> SkillTest a -> Int
modifiedTokenValue baseValue SkillTest {..} = foldr
  applyModifier
  baseValue
  skillTestModifiers
 where
  applyModifier (DoubleNegativeModifiersOnTokens _) n =
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
    PlaceCluePerPointOfFailureOnLocation iid -> case skillTestResult of
      FailedBy n -> s <$ unshiftMessage (PlaceCluesOnLocation iid n)
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
    FailSkillTest -> do
      unshiftMessages
        [ Ask skillTestInvestigator $ ChooseOne [SkillTestApplyResults]
        , SkillTestEnds
        ]
      pure $ s & result .~ FailedBy skillTestDifficulty
    StartSkillTest _ -> s <$ unshiftMessages
      (HashMap.foldMapWithKey
          (\k (i, _) -> [DiscardCard i k])
          skillTestCommittedCards
      <> [ InvestigatorStartSkillTest
             skillTestInvestigator
             skillTestAction
             skillTestSkillType
             skillTestModifiers
         ]
      )
    SkillTestCommitCard iid cardId -> do
      card <- asks (getCard iid cardId)
      pure $ s & committedCards %~ HashMap.insert cardId (iid, card)
    SkillTestUncommitCard _ cardId ->
      pure $ s & committedCards %~ HashMap.delete cardId
    AddModifier SkillTestTarget modifier ->
      pure $ s & modifiers %~ (modifier :)
    SkillTestEnds -> s <$ unshiftMessages
      [ RemoveAllModifiersOnTargetFrom
        (InvestigatorTarget skillTestInvestigator)
        SkillTestSource
      , ReturnTokens skillTestSetAsideTokens
      ]
    SkillTestResults -> do
      unshiftMessage
        (Ask skillTestInvestigator $ ChooseOne [SkillTestApplyResults])
      for_ skillTestCommittedCards $ \(iid, card) -> case card of
        PlayerCard MkPlayerCard {..} -> when
          (pcCardType == SkillType)
          (unshiftMessage (RunSkill iid pcCardCode skillTestResult))
        _ -> pure ()
      pure s
    SkillTestApplyResults -> do
      unshiftMessage SkillTestEnds

      case skillTestResult of
        SucceededBy _ -> unshiftMessages skillTestOnSuccess
        FailedBy _ -> unshiftMessages skillTestOnFailure
        Unrun -> pure ()

      unshiftMessages $ map
        (AddModifier (InvestigatorTarget skillTestInvestigator)
        . replaceModifierSource SkillTestSource
        )
        skillTestModifiers

      pure s
    RunSkillTest skillValue tokenValue -> do
      let tokenValue' = modifiedTokenValue tokenValue s
      let modifiedSkillValue' = skillValue + tokenValue' + skillIconCount s
      unshiftMessage SkillTestResults
      putStrLn
        . pack
        $ "Modified skill value: "
        <> show modifiedSkillValue'
        <> "\nDifficulty: "
        <> show skillTestDifficulty
      if modifiedSkillValue' >= skillTestDifficulty
        then pure $ s & result .~ SucceededBy
          (modifiedSkillValue' - skillTestDifficulty)
        else pure $ s & result .~ FailedBy
          (skillTestDifficulty - modifiedSkillValue')
    _ -> pure s
