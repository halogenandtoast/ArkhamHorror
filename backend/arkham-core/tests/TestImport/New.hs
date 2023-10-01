{-# OPTIONS_GHC -Wno-orphans #-}

module TestImport.New (module TestImport.New, module X) where

import TestImport.Lifted as X hiding (
  addToHand,
  drawCards,
  fightEnemy,
  investigate,
  loadDeck,
  moveTo,
  playCard,
  spawnAt,
 )

import Arkham.Ability.Types
import Arkham.Asset.Types
import Arkham.Classes.HasChaosTokenValue
import Arkham.Enemy.Types
import Arkham.Enemy.Types qualified as Field
import Arkham.GameEnv
import Arkham.Helpers.Message qualified as Helpers
import Arkham.Investigate.Types
import Arkham.Investigator.Types
import Arkham.Location.Types
import Arkham.Movement
import Arkham.Projection
import Arkham.SkillTest.Runner
import Arkham.SkillTestResult
import Arkham.Treachery.Types
import Arkham.Window (defaultWindows)
import GHC.Records
import Helpers.Message qualified

discard :: Targetable a => a -> TestAppT ()
discard = run . Discard GameSource . toTarget

click :: String -> TestAppT ()
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

instance HasField "abilities" AssetId (TestAppT [Ability]) where
  getField = field AssetAbilities

instance HasField "abilities" TreacheryId (TestAppT [Ability]) where
  getField = field TreacheryAbilities

instance HasField "horror" AssetId (TestAppT Int) where
  getField = field AssetHorror

instance HasField "skillValue" Investigator (TestAppT Int) where
  getField _ = getJustSkillTest >>= totalModifiedSkillValue

instance HasField "horror" Investigator (TestAppT Int) where
  getField = field InvestigatorHorror . toEntityId

instance HasField "damage" Enemy (TestAppT Int) where
  getField = field Field.EnemyDamage . toEntityId

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
