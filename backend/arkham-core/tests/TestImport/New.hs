{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module TestImport.New (module TestImport.New, module X) where

import TestImport.Lifted as X hiding (
  addToHand,
  drawCards,
  fightEnemy,
  investigate,
  loadDeck,
  moveTo,
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
import GHC.Records
import Helpers.Message qualified

click :: String -> TestAppT ()
click = chooseOnlyOption

loadDeck :: Investigator -> [CardDef] -> TestAppT ()
loadDeck i cs = run . LoadDeck (toId i) . Deck =<< traverse genPlayerCard cs

drawCards :: Investigator -> Int -> TestAppT ()
drawCards i n = do
  drawing <- Helpers.drawCards (toId i) i n
  run drawing

addToHand :: IsCard a => Investigator -> a -> TestAppT ()
addToHand i (toCard -> c) = run $ AddToHand (toId i) [c]

gainResources :: Investigator -> Int -> TestAppT ()
gainResources i n = run $ TakeResources (toId i) n (toSource i) False

loseActions :: Investigator -> Int -> TestAppT ()
loseActions i n = run $ LoseActions (toId i) (TestSource mempty) n

useAbility :: Investigator -> Ability -> TestAppT ()
useAbility i a = run $ UseAbility (toId i) a []

useReaction :: TestAppT ()
useReaction = chooseOptionMatching "use ability" \case
  AbilityLabel {} -> True
  _ -> False

setChaosTokens :: [ChaosTokenFace] -> TestAppT ()
setChaosTokens = run . SetChaosTokens

spawnAt :: Enemy -> Location -> TestAppT ()
spawnAt e l = run $ EnemySpawn Nothing (toId l) (toId e)

moveTo :: Investigator -> Location -> TestAppT ()
moveTo i l = run $ MoveTo $ move (toSource i) (toId i) (toId l)

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

instance HasField "clues" Investigator (TestAppT Int) where
  getField = field InvestigatorClues . toEntityId

instance HasField "clues" Location (TestAppT Int) where
  getField = field LocationClues . toEntityId

instance HasField "clues" TreacheryId (TestAppT Int) where
  getField = field TreacheryClues

instance HasField "resources" Investigator (TestAppT Int) where
  getField = field InvestigatorResources . toEntityId

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

instance HasField "skillValue" Investigator (TestAppT Int) where
  getField _ = getJustSkillTest >>= totalModifiedSkillValue

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

runSkillTest :: Investigator -> SkillType -> Int -> TestAppT ()
runSkillTest i st n = do
  run $ Helpers.Message.beginSkillTest i st n
  click "start skill test"

discoverClues :: Investigator -> Int -> TestAppT ()
discoverClues i n = do
  lid <- fieldJust InvestigatorLocation i.id
  run $ InvestigatorDiscoverClues i.id lid GameSource n Nothing
