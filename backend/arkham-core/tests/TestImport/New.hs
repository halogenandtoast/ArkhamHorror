{-# OPTIONS_GHC -Wno-orphans #-}

module TestImport.New (module TestImport.New, module X) where

import TestImport.Lifted as X hiding (
  addToHand,
  drawCards,
  duringTurn,
  evadedBy,
  evadedEnemy,
  fightEnemy,
  investigate,
  loadDeck,
  moveAllTo,
  moveTo,
  playCard,
  spawnAt,
 )

import Arkham.Ability (abilityActions)
import Arkham.Ability.Type (AbilityType (..))
import Arkham.Ability.Types
import Arkham.Action qualified as Action
import Arkham.Action.Additional
import Arkham.Agenda.Types
import Arkham.Asset.Types
import Arkham.Asset.Types qualified as Field
import Arkham.Asset.Uses
import Arkham.Classes.HasChaosTokenValue
import Arkham.Classes.HasGame
import Arkham.CommitRestriction
import Arkham.Deck qualified as Deck
import Arkham.Discover
import Arkham.Enemy.Types
import Arkham.Enemy.Types qualified as Field
import Arkham.Entities qualified as Entities
import Arkham.Game.Settings
import Arkham.GameEnv
import Arkham.Helpers.Investigator qualified as Helpers
import Arkham.Helpers.Message qualified as Helpers
import Arkham.Helpers.Use (asStartingUses)
import Arkham.Investigate.Types
import Arkham.Investigator.Types qualified as Field
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Types
import Arkham.Matcher qualified as Matcher
import Arkham.Movement
import Arkham.Name
import Arkham.Phase
import Arkham.Projection
import Arkham.SkillTest.Runner
import Arkham.SkillTestResult
import Arkham.Token (Token)
import Arkham.Token qualified as Token
import Arkham.Treachery.Types
import Arkham.Window (defaultWindows)
import Data.Text qualified as T
import GHC.Records
import GHC.TypeLits
import Helpers.Message qualified
import TestImport.Lifted qualified as Old

discard :: Targetable a => a -> TestAppT ()
discard = run . toDiscard GameSource

click :: HasCallStack => String -> TestAppT ()
click = chooseOnlyOption

loadDeck :: Investigator -> [CardDef] -> TestAppT ()
loadDeck i cs = run . LoadDeck (toId i) . Deck =<< traverse genPlayerCard cs

loadDeckCards :: Investigator -> [PlayerCard] -> TestAppT ()
loadDeckCards i = run . LoadDeck (toId i) . Deck

drawCards :: Investigator -> Int -> TestAppT ()
drawCards i n = run $ Helpers.drawCards (toId i) i n

playCard :: Investigator -> Card -> TestAppT ()
playCard i c = run $ InitiatePlayCard (toId i) c Nothing NoPayment (defaultWindows $ toId i) True

addToHand :: IsCard a => Investigator -> a -> TestAppT ()
addToHand i (toCard -> c) = run $ AddToHand (toId i) [c]

gainResources :: Investigator -> Int -> TestAppT ()
gainResources i n = run $ TakeResources (toId i) n (toSource i) False

spendResources :: Investigator -> Int -> TestAppT ()
spendResources i n = run $ SpendResources (toId i) n

loseActions :: Investigator -> Int -> TestAppT ()
loseActions i n = run $ LoseActions (toId i) (TestSource mempty) n

useAbility :: Investigator -> Ability -> TestAppT ()
useAbility i a = run $ UseAbility (toId i) a []

clickLabel :: Text -> TestAppT ()
clickLabel txt = chooseOptionMatching (T.unpack txt) \case
  Label label _ -> label == txt
  _ -> False

useReaction :: HasCallStack => TestAppT ()
useReaction = chooseOptionMatching "use reaction ability" \case
  AbilityLabel {ability} -> case abilityType ability of
    ReactionAbility {} -> True
    _ -> False
  _ -> False

useReactionOf :: (HasCallStack, Sourceable source) => source -> TestAppT ()
useReactionOf (toSource -> source) = chooseOptionMatching "use reaction ability" \case
  AbilityLabel {ability} -> case abilityType ability of
    ReactionAbility {} -> abilitySource ability == source
    _ -> False
  _ -> False

-- N.B. This won't work for multiple assets for the same card type
useFastActionOf :: (HasCallStack, Sourceable source) => source -> Int -> TestAppT ()
useFastActionOf (toSource -> source) idx = chooseOptionMatching "use fast action" \case
  AbilityLabel {ability} -> abilityIndex ability == idx && abilitySource ability == source
  _ -> False

genMyCard :: Investigator -> CardDef -> TestAppT Card
genMyCard self cardDef = do
  card <- genCard cardDef
  pure $ case card of
    PlayerCard pc -> PlayerCard (pc {pcOwner = Just (toId self)})
    other -> other

useForcedAbility :: HasCallStack => TestAppT ()
useForcedAbility = chooseOptionMatching "use forced ability" \case
  AbilityLabel {ability} -> case abilityType ability of
    ForcedAbility {} -> True
    _ -> False
  _ -> False

setChaosTokens :: [ChaosTokenFace] -> TestAppT ()
setChaosTokens = run . SetChaosTokens

spawnAt :: Enemy -> Location -> TestAppT ()
spawnAt e l = run $ EnemySpawnAtLocationMatching Nothing (Matcher.LocationWithId $ toId l) (toId e)

class CanMoveTo a where
  moveTo :: Investigator -> a -> TestAppT ()

instance CanMoveTo Location where
  moveTo i l = moveTo i (toId l)

instance CanMoveTo LocationId where
  moveTo i l = run $ Move $ move (toSource i) (toId i) l

fightEnemy :: Investigator -> Enemy -> TestAppT ()
fightEnemy i e = run $ FightEnemy (toId i) (toId e) (toSource i) Nothing SkillCombat False

evadeEnemy :: Investigator -> Enemy -> TestAppT ()
evadeEnemy i e = run $ EvadeEnemy (toId i) (toId e) (toSource i) Nothing SkillAgility False

evadedEnemy :: Investigator -> Enemy -> TestAppT ()
evadedEnemy i e = run $ EnemyEvaded (toId i) (toId e)

investigate :: Investigator -> Location -> TestAppT ()
investigate i l =
  run
    $ Investigate
    $ MkInvestigate
      { investigateInvestigator = toId i
      , investigateLocation = toId l
      , investigateSkillType = SkillIntellect
      , investigateSource = TestSource mempty
      , investigateTarget = Nothing
      , investigateIsAction = False
      }

instance HasField "engagedEnemies" Investigator (TestAppT [EnemyId]) where
  getField self = select $ Matcher.enemyEngagedWith $ toId self

instance HasField "playableCards" Investigator (TestAppT [Card]) where
  getField self = getPlayableCards (toAttrs self) (UnpaidCost NoAction) (defaultWindows $ toId self)

instance HasField "arcaneSlots" Investigator (TestAppT [Slot]) where
  getField = fieldMap InvestigatorSlots (findWithDefault [] #arcane) . toId

instance HasField "discard" Investigator (TestAppT [PlayerCard]) where
  getField = field InvestigatorDiscard . toEntityId

instance HasField "bonded" Investigator (TestAppT [Card]) where
  getField = field InvestigatorBondedCards . toEntityId

instance HasField "deck" Investigator (TestAppT (Deck PlayerCard)) where
  getField = field InvestigatorDeck . toEntityId

instance HasField "willpower" Investigator (TestAppT Int) where
  getField i = willpower <$> Helpers.modifiedStatsOf Nothing (toId i)

instance HasField "intellect" Investigator (TestAppT Int) where
  getField i = intellect <$> Helpers.modifiedStatsOf Nothing (toId i)

instance HasField "agility" Investigator (TestAppT Int) where
  getField i = agility <$> Helpers.modifiedStatsOf Nothing (toId i)

instance HasField "combat" Investigator (TestAppT Int) where
  getField i = combat <$> Helpers.modifiedStatsOf Nothing (toId i)

instance HasField "clues" Investigator (TestAppT Int) where
  getField = field InvestigatorClues . toEntityId

instance HasField "accessibleLocations" Investigator (TestAppT [LocationId]) where
  getField self = do
    run $ SetActiveInvestigator (toId self)
    mlid <- self.location
    case mlid of
      Nothing -> pure []
      Just lid ->
        select
          $ Matcher.canEnterLocation (toId self)
          <> Matcher.AccessibleFrom (Matcher.LocationWithId lid)

instance HasField "clues" Location (TestAppT Int) where
  getField = field LocationClues . toEntityId

instance HasField "connectedLocations" LocationId (TestAppT [LocationId]) where
  getField = select . Matcher.ConnectedTo . Matcher.LocationWithId

instance HasField "connectedLocations" Location (TestAppT [LocationId]) where
  getField = select . Matcher.ConnectedTo . Matcher.LocationWithId . toId

instance HasField "clues" TreacheryId (TestAppT Int) where
  getField = field TreacheryClues

instance HasField "resources" TreacheryId (TestAppT Int) where
  getField = field TreacheryResources

instance HasField "resources" Investigator (TestAppT Int) where
  getField = field InvestigatorResources . toEntityId

instance HasField "slots" Investigator (TestAppT (Map SlotType [Slot])) where
  getField = field InvestigatorSlots . toEntityId

instance HasField "mentalTrauma" Investigator (TestAppT Int) where
  getField = field InvestigatorMentalTrauma . toEntityId

instance HasField "elderSignModifier" Investigator (TestAppT ChaosTokenModifier) where
  getField i =
    fmap (\(ChaosTokenValue _ ctm) -> ctm)
      . getChaosTokenValue (toId i) ElderSign
      =<< getInvestigator (toId i)

instance HasField "location" Investigator (TestAppT (Maybe LocationId)) where
  getField = field InvestigatorLocation . toEntityId

instance HasField "defeated" Investigator (TestAppT Bool) where
  getField = (<=~> Matcher.DefeatedInvestigator) . toId

instance HasField "remainingActions" Investigator (TestAppT Int) where
  getField = field InvestigatorRemainingActions . toEntityId

instance HasField "additionalActions" Investigator (TestAppT [AdditionalAction]) where
  getField = field InvestigatorAdditionalActions . toEntityId

instance HasField "remainingSanity" Investigator (TestAppT Int) where
  getField = field InvestigatorRemainingSanity . toEntityId

instance HasField "remainingHealth" Investigator (TestAppT Int) where
  getField = field InvestigatorRemainingHealth . toEntityId

instance HasField "hand" Investigator (TestAppT [Card]) where
  getField = field InvestigatorHand . toEntityId

instance HasField "abilities" Investigator (TestAppT [Ability]) where
  getField = field InvestigatorAbilities . toEntityId

instance HasField "exhausted" Enemy (TestAppT Bool) where
  getField = fmap (enemyExhausted . toAttrs) . getEnemy . toEntityId

instance HasField "abilities" Enemy (TestAppT [Ability]) where
  getField = field EnemyAbilities . toEntityId

instance HasField "location" Enemy (TestAppT (Maybe LocationId)) where
  getField = field EnemyLocation . toEntityId

instance HasField "countTokens" AssetId (Token -> TestAppT Int) where
  getField aid tType = fieldMap AssetTokens (Token.countTokens tType) aid

instance HasField "abilities" AssetId (TestAppT [Ability]) where
  getField = field AssetAbilities

instance HasField "cardsUnderneath" AssetId (TestAppT [Card]) where
  getField = field AssetCardsUnderneath

instance HasField "owner" AssetId (TestAppT (Maybe InvestigatorId)) where
  getField = field AssetOwner

instance HasField "controller" AssetId (TestAppT (Maybe InvestigatorId)) where
  getField = field AssetController

instance HasField "abilities" TreacheryId (TestAppT [Ability]) where
  getField = field TreacheryAbilities

instance HasField "location" EnemyId (TestAppT (Maybe LocationId)) where
  getField = field EnemyLocation

instance HasField "doom" EnemyId (TestAppT Int) where
  getField = field EnemyDoom

instance HasField "doom" AssetId (TestAppT Int) where
  getField = field AssetDoom

instance HasField "doom" Agenda (TestAppT Int) where
  getField = field AgendaDoom . toEntityId

instance HasField "health" AssetId (TestAppT (Maybe Int)) where
  getField = field AssetRemainingHealth

instance HasField "sanity" AssetId (TestAppT (Maybe Int)) where
  getField = field AssetRemainingSanity

instance HasField "horror" AssetId (TestAppT Int) where
  getField = field AssetHorror

instance HasField "uses" AssetId (TestAppT (Map UseType Int)) where
  getField = field AssetUses

instance HasField "ammo" AssetId (TestAppT Int) where
  getField = fieldMap AssetUses (findWithDefault 0 Ammo)

instance HasField "charges" AssetId (TestAppT Int) where
  getField = fieldMap AssetUses (findWithDefault 0 Charge)

instance HasField "secrets" AssetId (TestAppT Int) where
  getField = fieldMap AssetUses (findWithDefault 0 Secret)

instance HasField "damage" AssetId (TestAppT Int) where
  getField = field Field.AssetDamage

instance HasField "exhausted" AssetId (TestAppT Bool) where
  getField = field AssetExhausted

instance HasField "skillValue" Investigator (TestAppT Int) where
  getField _ = getJustSkillTest >>= totalModifiedSkillValue

instance HasField "horror" Investigator (TestAppT Int) where
  getField = field InvestigatorHorror . toEntityId

instance HasField "damage" Enemy (TestAppT Int) where
  getField = field Field.EnemyDamage . toEntityId

instance HasField "damage" Investigator (TestAppT Int) where
  getField = field Field.InvestigatorDamage . toEntityId

addHorror :: Investigator -> Int -> TestAppT ()
addHorror i n = do
  run $ InvestigatorDirectDamage (toId i) (TestSource mempty) 0 n
  replicateM_ n $ click "apply horror"

assertPassedSkillTest :: TestAppT ()
assertPassedSkillTest = do
  st <- getJustSkillTest
  case skillTestResult st of
    SucceededBy {} -> pure ()
    FailedBy {} -> expectationFailure "Expected skill test to pass, but failed"
    Unrun {} -> expectationFailure "Expected skill test to pass, but is unrun"

assertFailedSkillTest :: TestAppT ()
assertFailedSkillTest = do
  st <- getJustSkillTest
  case skillTestResult st of
    FailedBy {} -> pure ()
    SucceededBy {} -> expectationFailure "Expected skill test to fail, but passed"
    Unrun {} -> expectationFailure "Expected skill test to pass, but is unrun"

runSkillTest :: HasCallStack => Investigator -> SkillType -> Int -> TestAppT ()
runSkillTest i st n = do
  run $ Helpers.Message.beginSkillTest i st n
  click "start skill test"

discoverClues :: Investigator -> Int -> TestAppT ()
discoverClues i n = do
  lid <- fieldJust InvestigatorLocation i.id
  run $ DiscoverClues i.id $ discover lid GameSource n

getActionsFrom :: Sourceable source => Investigator -> source -> TestAppT [Ability]
getActionsFrom i s = do
  actions <- getActions (toId i) (defaultWindows $ toId i)
  pure $ filter ((== toSource s) . abilitySource) actions

hasDiscardPile :: Investigator -> [CardDef] -> TestAppT ()
hasDiscardPile i cs = i.discard `shouldSatisfyM` ((== cs) . map toCardDef)

hasDeck :: Investigator -> [CardDef] -> TestAppT ()
hasDeck i cs = i.deck `shouldSatisfyM` ((== cs) . map toCardDef . unDeck)

asDefs :: (MonoFoldable (t a), HasCardDef (Element (t a))) => TestAppT (t a) -> TestAppT [CardDef]
asDefs action = map toCardDef . toList <$> action

errata :: String -> SpecWith a -> SpecWith a
errata s = context ("[ERRATA]: " <> s)

faq :: String -> SpecWith a -> SpecWith a
faq s = context ("[FAQ]: " <> s)

attackedBy :: Investigator -> Enemy -> TestAppT ()
attackedBy i = run . enemyAttack i

assertHasNoReaction :: TestAppT ()
assertHasNoReaction = do
  questionMap <- gameQuestion <$> getGame
  let
    isReaction = \case
      AbilityLabel {} -> True
      _ -> False
  case mapToList questionMap of
    [(_, question)] -> case question of
      ChooseOne msgs -> case find isReaction msgs of
        Just msg -> expectationFailure $ "expected no reaction, but found " <> show msg
        Nothing -> pure ()
      ChooseN _ msgs -> case find isReaction msgs of
        Just msg -> expectationFailure $ "expected no reaction, but found " <> show msg
        Nothing -> pure ()
      _ -> pure ()
    _ -> pure ()

drawsCard :: Investigator -> CardDef -> TestAppT ()
drawsCard i cd = do
  c <- genCard cd
  let drawing = Helpers.drawCards (toId i) GameSource 1
  runAll [PutCardOnTopOfDeck (toId i) (Deck.InvestigatorDeck $ toId i) c, drawing]

startSkillTest :: HasCallStack => TestAppT ()
startSkillTest = chooseOptionMatching "start skill test" \case
  StartSkillTestButton {} -> True
  _ -> False

applyResults :: TestAppT ()
applyResults = chooseOptionMatching "apply skill test results" \case
  SkillTestApplyResultsButton {} -> True
  _ -> False

inWindow :: Investigator -> TestAppT () -> TestAppT ()
inWindow self body = do
  run $ CheckWindow [toId self] (defaultWindows $ toId self)
  body

chooseTarget :: (HasCallStack, Targetable target) => target -> TestAppT ()
chooseTarget (toTarget -> target) =
  chooseOptionMatching "choose target" \case
    TargetLabel target' _ -> target == target'
    FightLabel eid _ -> case target of
      EnemyTarget eid' -> eid == eid'
      _ -> False
    EvadeLabel eid _ -> case target of
      EnemyTarget eid' -> eid == eid'
      _ -> False
    _ -> False

chooseSkill :: HasCallStack => SkillType -> TestAppT ()
chooseSkill sType =
  chooseOptionMatching "choose self" \case
    SkillLabel sType' _ -> sType == sType'
    Label lbl _ -> lookup lbl labeledSkills == Just sType
    _ -> False

evadedBy :: Enemy -> Investigator -> TestAppT Bool
evadedBy enemy' _investigator = fieldP EnemyEngagedInvestigators null (toId enemy')

takeResource :: Investigator -> TestAppT ()
takeResource self = run $ TakeResources (toId self) 1 (toSource self) True

duringTurn :: Investigator -> TestAppT () -> TestAppT ()
duringTurn self body = do
  run $ BeginTurn (toId self)
  body
  run $ ChooseEndTurn (toId self)

duringPhase :: Phase -> TestAppT () -> TestAppT ()
duringPhase phase body = do
  run $ Begin phase
  body
  case phase of
    InvestigationPhase -> run EndInvestigation
    _ -> run EndPhase

duringTurnWindow :: Investigator -> Window
duringTurnWindow = Old.duringTurn . toId

class Attacks a b where
  attacks :: a -> b -> TestAppT ()

instance Attacks Enemy Investigator where
  attacks e i = run $ enemyAttack i e

withRewind :: TestAppT () -> TestAppT ()
withRewind action = do
  original <- get
  liftIO $ do
    testApp <- cloneTestApp original
    runTestApp testApp action

assertRunsMessage :: Message -> TestAppT () -> TestAppT ()
assertRunsMessage msg body = do
  didRunMessage <- createMessageMatcher msg
  body
  didRunMessage `refShouldBe` True

assertDoesNotRunMessage :: Message -> TestAppT () -> TestAppT ()
assertDoesNotRunMessage msg body = do
  didRunMessage <- createMessageMatcher msg
  body
  didRunMessage `refShouldBe` False

class Gives (s :: Symbol) where
  gives :: KnownSymbol s => CardDef -> Int -> SpecWith ()

instance Gives "agility" where
  gives = \def n -> it ("gives +" <> show n <> " agility") . gameTest $ \self -> do
    withProp @"agility" 0 self
    self `putCardIntoPlay` def
    self.agility `shouldReturn` n

instance Gives "willpower" where
  gives = \def n -> it ("gives +" <> show n <> " willpower") . gameTest $ \self -> do
    withProp @"willpower" 0 self
    self `putCardIntoPlay` def
    self.willpower `shouldReturn` n

instance Gives "intellect" where
  gives = \def n -> it ("gives +" <> show n <> " intellect") . gameTest $ \self -> do
    withProp @"intellect" 0 self
    self `putCardIntoPlay` def
    self.intellect `shouldReturn` n

instance Gives "combat" where
  gives = \def n -> it ("gives +" <> show n <> " combat") . gameTest $ \self -> do
    withProp @"combat" 0 self
    self `putCardIntoPlay` def
    self.combat `shouldReturn` n

class HasUses (s :: Symbol) where
  hasUses :: KnownSymbol s => CardDef -> Int -> SpecWith ()

instance HasUses "charge" where
  hasUses = \def n -> it ("starts with " <> show n <> " charges") . gameTest $ \self -> do
    this <- self `putAssetIntoPlay` def
    fieldMap AssetUses (findWithDefault 0 Charge) this `shouldReturn` n

instance HasUses "ammo" where
  hasUses = \def n -> it ("starts with " <> show n <> " ammo") . gameTest $ \self -> do
    this <- self `putAssetIntoPlay` def
    fieldMap AssetUses (findWithDefault 0 Ammo) this `shouldReturn` n

instance HasUses "supply" where
  hasUses = \def n -> it ("starts with " <> show n <> " supplies") . gameTest $ \self -> do
    this <- self `putAssetIntoPlay` def
    fieldMap AssetUses (findWithDefault 0 Supply) this `shouldReturn` n

instance HasUses "secret" where
  hasUses = \def n -> it ("starts with " <> show n <> " secrets") . gameTest $ \self -> do
    this <- self `putAssetIntoPlay` def
    fieldMap AssetUses (findWithDefault 0 Secret) this `shouldReturn` n

instance HasUses "supplies" where
  hasUses = \def n -> it ("starts with " <> show n <> " supplies") . gameTest $ \self -> do
    this <- self `putAssetIntoPlay` def
    fieldMap AssetUses (findWithDefault 0 Supply) this `shouldReturn` n

instance HasUses "bounties" where
  hasUses = \def n -> it ("starts with " <> show n <> " bounties") . gameTest $ \self -> do
    this <- self `putAssetIntoPlay` def
    fieldMap AssetUses (findWithDefault 0 Bounty) this `shouldReturn` n

-- N.B. Use carefully as this deletes the entire question currently, but it is a useful way to make
-- sure we've applied all damage
applyAllDamage :: TestAppT ()
applyAllDamage = do
  questionMap <- gameQuestion <$> getGame
  let
    choices = case mapToList questionMap of
      [(_, question)] -> case question of
        ChooseOne msgs -> msgs
        _ -> []
      _ -> []
    isDamage = \case
      ComponentLabel (InvestigatorComponent _ DamageToken) _ -> True
      ComponentLabel (AssetComponent _ DamageToken) _ -> True
      _ -> False
  case find isDamage choices of
    Nothing -> pure ()
    Just msg -> do
      overTest (questionL .~ mempty)
      push (uiToRun msg) >> runMessages >> applyAllDamage

applyAllHorror :: TestAppT ()
applyAllHorror = do
  questionMap <- gameQuestion <$> getGame
  let
    choices = case mapToList questionMap of
      [(_, question)] -> case question of
        ChooseOne msgs -> msgs
        _ -> []
      _ -> []
    isHorror = \case
      ComponentLabel (InvestigatorComponent _ HorrorToken) _ -> True
      ComponentLabel (AssetComponent _ HorrorToken) _ -> True
      _ -> False
  case find isHorror choices of
    Nothing -> pure ()
    Just msg -> do
      overTest (questionL .~ mempty)
      push (uiToRun msg) >> runMessages >> applyAllHorror

discardedWhenNoUses :: CardDef -> SpecWith ()
discardedWhenNoUses def = it "is discarded when no uses" . gameTest $ \self -> do
  this <- self `putAssetIntoPlay` def
  uses <- fieldMapM AssetStartingUses asStartingUses this
  case useType uses of
    Nothing -> expectationFailure "asset has no uses"
    Just useType' -> do
      let useCount' = useCount uses
      run $ SpendUses (toTarget this) useType' useCount'
  assert $ selectNone $ Matcher.assetIs def
  asDefs self.discard `shouldReturn` [def]

isFastAsset :: CardDef -> SpecWith ()
isFastAsset def = it "is fast" (cdFastWindow def `shouldBe` Just (Matcher.DuringTurn Matcher.You) :: IO ())

isHunter :: CardDef -> SpecWith ()
isHunter def = it "is hunter" (cdKeywords def `shouldSatisfy` elem Keyword.Hunter :: IO ())

isAloof :: CardDef -> SpecWith ()
isAloof def = it "is hunter" (cdKeywords def `shouldSatisfy` elem Keyword.Aloof :: IO ())

withEach :: [a] -> (a -> TestAppT ()) -> TestAppT ()
withEach xs f = for_ xs $ withRewind . f

commit :: (HasCallStack, IsCard card) => card -> TestAppT ()
commit = chooseTarget . toCardId

assertNoReaction :: TestAppT ()
assertNoReaction = do
  questionMap <- gameQuestion <$> getGame
  let
    choices = case mapToList questionMap of
      [(_, question)] -> case question of
        ChooseOne msgs -> msgs
        _ -> []
      _ -> []
    isReaction = \case
      AbilityLabel {ability} -> case abilityType ability of
        ReactionAbility {} -> True
        _ -> False
      _ -> False
  case find isReaction choices of
    Nothing -> pure ()
    Just choice -> expectationFailure $ "expected no reaction, but found:\n\n" <> show choice

moveAllTo :: Location -> TestAppT ()
moveAllTo = run . Old.moveAllTo

assertTarget :: (HasCallStack, Targetable target) => target -> TestAppT ()
assertTarget (toTarget -> target) = do
  questionMap <- gameQuestion <$> getGame
  let
    choices =
      case mapToList questionMap of
        [(_, question)] -> case question of
          ChooseOne msgs -> msgs
          ChooseN _ msgs -> msgs
          _ -> error $ "unsupported questions type: " <> show question
        _ -> error "There must be only one question to use this function"
    isMessageTarget = \case
      TargetLabel target' _ -> target == target'
      FightLabel eid _ -> case target of
        EnemyTarget eid' -> eid == eid'
        _ -> False
      _ -> False

  case find isMessageTarget choices of
    Nothing -> expectationFailure $ "expected to find target " <> show target <> " but did not"
    Just _ -> pure ()

assertNotTarget :: (HasCallStack, Targetable target) => target -> TestAppT ()
assertNotTarget (toTarget -> target) = do
  questionMap <- gameQuestion <$> getGame
  let
    choices =
      case mapToList questionMap of
        [(_, question)] -> case question of
          ChooseOne msgs -> msgs
          ChooseN _ msgs -> msgs
          _ -> error $ "unsupported questions type: " <> show question
        _ -> []
    isMessageTarget = \case
      TargetLabel target' _ -> target == target'
      FightLabel eid _ -> case target of
        EnemyTarget eid' -> eid == eid'
        _ -> False
      _ -> False

  case find isMessageTarget choices of
    Nothing -> pure ()
    Just _ -> expectationFailure $ "expected not to find target " <> show target <> " but did"

unlessSetting :: (Settings -> Bool) -> TestAppT () -> TestAppT ()
unlessSetting f body = do
  setting <- getSettings
  unless (f setting) body

failSkillTest :: Investigator -> TestAppT ()
failSkillTest self = do
  setChaosTokens [AutoFail]
  runSkillTest self #combat 1
  applyResults

maxCommittedPerSkillTest :: Int -> CardDef -> SpecWith ()
maxCommittedPerSkillTest n def =
  it ("Max " <> show n <> " committed per skill test") $ do
    (cdCommitRestrictions def `shouldSatisfy` elem MaxOnePerTest :: IO ())

-- Note: at the moment we don't have a better way of knowing if damage is
-- direct of not aside from checking for the asset filter
assertDamageIsDirect :: TestAppT ()
assertDamageIsDirect = do
  questionMap <- gameQuestion <$> getGame
  let
    choices = case mapToList questionMap of
      [(_, question)] -> case question of
        ChooseOne msgs -> msgs
        _ -> []
      _ -> []
    isDirectDamage = \case
      ComponentLabel (InvestigatorComponent _ DamageToken) msgs -> flip any msgs $ \case
        InvestigatorDoAssignDamage _ _ _ (Matcher.AssetWithModifier CanBeAssignedDirectDamage) _ _ _ _ -> True
        _ -> False
      _ -> False
  unless (any isDirectDamage choices) $ expectationFailure "expected damage to be direct"

-- Note: at the moment we don't have a better way of knowing if damage is
-- direct of not aside from checking for the asset filter
assertHorrorIsDirect :: TestAppT ()
assertHorrorIsDirect = do
  questionMap <- gameQuestion <$> getGame
  let
    choices = case mapToList questionMap of
      [(_, question)] -> case question of
        ChooseOne msgs -> msgs
        _ -> []
      _ -> []
    isDirectHorror = \case
      ComponentLabel (InvestigatorComponent _ HorrorToken) msgs -> flip any msgs $ \case
        InvestigatorDoAssignDamage _ _ _ (Matcher.AssetWithModifier CanBeAssignedDirectDamage) _ _ _ _ -> True
        _ -> False
      _ -> False
  unless (any isDirectHorror choices) $ expectationFailure "expected horror to be direct"

createWeaknessEnemy :: Investigator -> CardDef -> TestAppT Enemy
createWeaknessEnemy self def = do
  card <- genCard def
  enemyId <- getRandom
  let enemy' =
        overAttrs (\attrs -> attrs {enemyBearer = Just (toId self)})
          $ lookupEnemy (toCardCode card) enemyId (toCardId card)
  overTest $ entitiesL . Entities.enemiesL %~ insertEntity enemy'
  pure enemy'

assertChanges :: (Eq a, Show a) => TestAppT a -> a -> a -> TestAppT () -> TestAppT ()
assertChanges action a b body = do
  action `shouldReturn` a
  body
  action `shouldReturn` b

-- While this function primarily exists to resolve the amounts which you could
-- call directly, it also does a bunch of verification on the test import to
-- make sure it coincides with the actual amounts and limit
resolveAmounts :: Investigator -> [(Text, Int)] -> TestAppT ()
resolveAmounts self choices = do
  questionMap <- gameQuestion <$> getGame
  ChooseAmounts _ targetValue availableChoices target <- case mapToList questionMap of
    [(_, question)] -> case question of
      ChooseAmounts {} -> pure question
      _ -> error $ "expected ChooseAmounts, but got: " <> show question
    _ -> error "expected one question"

  for_ choices $ \(lbl, value) -> do
    case find (\(AmountChoice lbl' _ _) -> lbl == lbl') availableChoices of
      Nothing -> error $ "expected to find " <> show lbl <> " in " <> show availableChoices
      Just (AmountChoice _ minVal maxVal) -> do
        when (value < minVal)
          $ error
          $ "expected "
          <> show value
          <> " to be >= "
          <> show minVal
        when (value > maxVal)
          $ error
          $ "expected "
          <> show value
          <> " to be <= "
          <> show maxVal

  let total = sum $ map snd choices

  case targetValue of
    MaxAmountTarget n -> when (total > n) $ expectationFailure $ "expected " <> show total <> " to be <= " <> show n
    TotalAmountTarget n -> when (total /= n) $ expectationFailure $ "expected " <> show total <> " to be == " <> show n
    MinAmountTarget n -> when (total < n) $ expectationFailure $ "expected " <> show total <> " to be == " <> show n
    AmountOneOf ns ->
      when (total `notElem` ns)
        $ expectationFailure
        $ "expected "
        <> show total
        <> " to be in "
        <> show ns

  run $ ResolveAmounts (toId self) choices target

chooseFight :: TestAppT ()
chooseFight = do
  chooseOptionMatching "choose fight" \case
    AbilityLabel _ ability _ _ -> Action.Fight `elem` abilityActions ability
    _ -> False

assertMaxAmountChoice :: HasCallStack => Int -> TestAppT ()
assertMaxAmountChoice n = do
  questionMap <- gameQuestion <$> getGame
  ChooseAmounts _ targetValue _ _ <- case mapToList questionMap of
    [(_, question)] -> case question of
      ChooseAmounts {} -> pure question
      _ -> error $ "expected ChooseAmounts, but got: " <> show question
    _ -> error "expected one question"

  case targetValue of
    MaxAmountTarget n' -> n' `shouldBe` n
    MinAmountTarget _ -> expectationFailure "expected MaxAmountTarget"
    TotalAmountTarget _ -> expectationFailure "expected MaxAmountTarget"
    AmountOneOf _ -> expectationFailure "expected MaxAmountTarget"

beginsWithInPlay :: CardDef -> CardDef -> SpecWith ()
beginsWithInPlay investigator card = it ("begins with " <> T.unpack (toTitle card) <> " in play") . gameTestWith investigator $ \self -> do
  cards <- testPlayerCards 20
  assetCard <- genPlayerCard card
  withProp @"deck" (Deck (assetCard : cards)) self
  run $ SetupInvestigator (toId self)
  assertAny $ Matcher.assetIs card

setActive :: Investigator -> TestAppT ()
setActive player = run $ SetActiveInvestigator (toId player)
