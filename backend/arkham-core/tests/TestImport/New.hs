{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module TestImport.New (module TestImport.New, module X) where

import TestImport.Lifted as X hiding (
  addToHand,
  drawCards,
  duringTurn,
  evadedBy,
  fightEnemy,
  investigate,
  loadDeck,
  moveTo,
  playCard,
  spawnAt,
 )

import Arkham.Ability.Types
import Arkham.Asset.Types
import Arkham.Asset.Types qualified as Field
import Arkham.Asset.Uses
import Arkham.Classes.HasChaosTokenValue
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Types
import Arkham.Enemy.Types qualified as Field
import Arkham.GameEnv
import Arkham.Helpers.Investigator qualified as Helpers
import Arkham.Helpers.Message qualified as Helpers
import Arkham.Investigate.Types
import Arkham.Investigator.Types
import Arkham.Investigator.Types qualified as Field
import Arkham.Location.Types
import Arkham.Matcher qualified as Matcher
import Arkham.Movement
import Arkham.Projection
import Arkham.SkillTest.Runner
import Arkham.SkillTestResult
import Arkham.Treachery.Types
import Arkham.Window (defaultWindows)
import GHC.Records
import GHC.TypeLits
import Helpers.Message qualified
import TestImport.Lifted qualified as Old

discard :: Targetable a => a -> TestAppT ()
discard = run . Discard GameSource . toTarget

click :: HasCallStack => String -> TestAppT ()
click = chooseOnlyOption

loadDeck :: Investigator -> [CardDef] -> TestAppT ()
loadDeck i cs = run . LoadDeck (toId i) . Deck =<< traverse genPlayerCard cs

drawCards :: Investigator -> Int -> TestAppT ()
drawCards i n = do
  drawing <- Helpers.drawCards (toId i) i n
  run drawing

playCard :: Investigator -> Card -> TestAppT ()
playCard i c = run $ InitiatePlayCard (toId i) c Nothing (defaultWindows $ toId i) True

addToHand :: IsCard a => Investigator -> a -> TestAppT ()
addToHand i (toCard -> c) = run $ AddToHand (toId i) [c]

gainResources :: Investigator -> Int -> TestAppT ()
gainResources i n = run $ TakeResources (toId i) n (toSource i) False

loseActions :: Investigator -> Int -> TestAppT ()
loseActions i n = run $ LoseActions (toId i) (TestSource mempty) n

useAbility :: Investigator -> Ability -> TestAppT ()
useAbility i a = run $ UseAbility (toId i) a []

useReaction :: HasCallStack => TestAppT ()
useReaction = chooseOptionMatching "use reaction ability" \case
  AbilityLabel {} -> True
  _ -> False

-- N.B. This won't work for multiple assets for the same card type
useFastActionOf :: (HasCallStack, Sourceable source) => source -> Int -> TestAppT ()
useFastActionOf (toSource -> source) idx = chooseOptionMatching "use fast action" \case
  AbilityLabel {ability} -> abilityIndex ability == idx && abilitySource ability == source
  _ -> False

useForcedAbility :: HasCallStack => TestAppT ()
useForcedAbility = chooseOptionMatching "use forced ability" \case
  AbilityLabel {} -> True
  _ -> False

setChaosTokens :: [ChaosTokenFace] -> TestAppT ()
setChaosTokens = run . SetChaosTokens

spawnAt :: Enemy -> Location -> TestAppT ()
spawnAt e l = run $ EnemySpawn Nothing (toId l) (toId e)

moveTo :: Investigator -> Location -> TestAppT ()
moveTo i l = run $ Move $ move (toSource i) (toId i) (toId l)

fightEnemy :: Investigator -> Enemy -> TestAppT ()
fightEnemy i e = run $ FightEnemy (toId i) (toId e) (toSource i) Nothing SkillCombat False

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

instance HasField "playableCards" Investigator (TestAppT [Card]) where
  getField self = getPlayableCards (toAttrs self) UnpaidCost (defaultWindows $ toId self)

instance HasField "discard" Investigator (TestAppT [PlayerCard]) where
  getField = field InvestigatorDiscard . toEntityId

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

instance HasField "clues" Location (TestAppT Int) where
  getField = field LocationClues . toEntityId

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

instance HasField "remainingActions" Investigator (TestAppT Int) where
  getField = field InvestigatorRemainingActions . toEntityId

instance HasField "hand" Investigator (TestAppT [Card]) where
  getField = field InvestigatorHand . toEntityId

instance HasField "abilities" Investigator (TestAppT [Ability]) where
  getField = field InvestigatorAbilities . toEntityId

instance HasField "abilities" Enemy (TestAppT [Ability]) where
  getField = field EnemyAbilities . toEntityId

instance HasField "abilities" AssetId (TestAppT [Ability]) where
  getField = field AssetAbilities

instance HasField "abilities" TreacheryId (TestAppT [Ability]) where
  getField = field TreacheryAbilities

instance HasField "horror" AssetId (TestAppT Int) where
  getField = field AssetHorror

instance HasField "uses" AssetId (TestAppT (Uses Int)) where
  getField = field AssetUses

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

runSkillTest :: Investigator -> SkillType -> Int -> TestAppT ()
runSkillTest i st n = do
  run $ Helpers.Message.beginSkillTest i st n
  click "start skill test"

discoverClues :: Investigator -> Int -> TestAppT ()
discoverClues i n = do
  lid <- fieldJust InvestigatorLocation i.id
  run $ InvestigatorDiscoverClues i.id lid GameSource n Nothing

getActionsFrom :: Sourceable source => Investigator -> source -> TestAppT [Ability]
getActionsFrom i s = do
  actions <- nub <$> concatMapM (getActions (toId i)) (defaultWindows $ toId i)
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
  drawing <- Helpers.drawCards (toId i) GameSource 1
  runAll [PutCardOnTopOfDeck (toId i) (Deck.InvestigatorDeck $ toId i) c, drawing]

startSkillTest :: TestAppT ()
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

chooseTarget :: HasCallStack => Targetable target => target -> TestAppT ()
chooseTarget (toTarget -> target) =
  chooseOptionMatching "choose self" \case
    TargetLabel target' _ -> target == target'
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

skip :: TestAppT ()
skip = chooseOptionMatching "skip" \case
  Label _ [] -> True
  _ -> False

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

instance HasUses "ammo" where
  hasUses = \def n -> it ("starts with " <> show n <> " ammo") . gameTest $ \self -> do
    this <- self `putAssetIntoPlay` def
    field AssetUses this `shouldReturn` Uses Ammo n

instance HasUses "supplies" where
  hasUses = \def n -> it ("starts with " <> show n <> " supplies") . gameTest $ \self -> do
    this <- self `putAssetIntoPlay` def
    field AssetUses this `shouldReturn` Uses Supply n

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

discardedWhenNoUses :: CardDef -> SpecWith ()
discardedWhenNoUses def = it "is discarded when no uses" . gameTest $ \self -> do
  this <- self `putAssetIntoPlay` def
  uses <- field AssetUses this
  case useType uses of
    Nothing -> expectationFailure "asset has no uses"
    Just useType' -> do
      let useCount' = useCount uses
      run $ SpendUses (toTarget this) useType' useCount'
  assert $ selectNone $ Matcher.assetIs def
  asDefs self.discard `shouldReturn` [def]

isFastAsset :: CardDef -> SpecWith ()
isFastAsset def = it "is fast" $ (cdFastWindow def `shouldBe` Just (Matcher.DuringTurn Matcher.You) :: IO ())

withEach :: [a] -> (a -> TestAppT ()) -> TestAppT ()
withEach xs f = for_ xs $ withRewind . f
