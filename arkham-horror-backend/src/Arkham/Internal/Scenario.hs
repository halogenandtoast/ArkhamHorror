module Arkham.Internal.Scenario
  ( toInternalScenario
  , drawCard
  )
where

import Arkham.Constructors
import Arkham.Entity.ArkhamGame
import Arkham.Internal.Act
import Arkham.Internal.Agenda
import Arkham.Internal.ChaosToken
import Arkham.Internal.EncounterCard
import Arkham.Internal.Investigator
import Arkham.Internal.Location
import Arkham.Internal.Types
import Arkham.Types
import Arkham.Types.Card
import Arkham.Types.ChaosToken
import Arkham.Types.Difficulty
import Arkham.Types.Game
import Arkham.Types.GameState
import Arkham.Types.Location
import Arkham.Types.Scenario
import Base.Lock
import ClassyPrelude
import Control.Monad.Random
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NE
import Lens.Micro
import Lens.Micro.Platform ()
import Safe hiding (at)

countTraitMatch :: ArkhamCardTrait -> ArkhamLocation -> Int
countTraitMatch _ _ = 0

toInternalScenario :: ArkhamGame -> ArkhamScenarioInternal
toInternalScenario g =
  fromJustNote "missing scenario"
    $ HashMap.lookup (asScenarioCode scenario') allScenarios
    <*> pure difficulty'
 where
  scenario' = g ^. scenario
  difficulty' = g ^. difficulty

allScenarios
  :: HashMap ArkhamScenarioCode (ArkhamDifficulty -> ArkhamScenarioInternal)
allScenarios =
  HashMap.fromList [(ArkhamScenarioCode "theGathering", theGathering)]

defaultTokenMap :: HashMap ArkhamChaosToken ArkhamChaosTokenInternal
defaultTokenMap = HashMap.fromList
  [ (PlusOne, plusOneToken)
  , (Zero, zeroToken)
  , (MinusOne, minusOneToken)
  , (MinusTwo, minusTwoToken)
  , (MinusThree, minusThreeToken)
  , (MinusFour, minusFourToken)
  , (MinusFive, minusFiveToken)
  , (MinusSix, minusSixToken)
  , (MinusSeven, minusSevenToken)
  , (MinusEight, minusEightToken)
  , (AutoFail, autoFailToken)
  , (ElderSign, elderSignToken)
  ]

buildTokenMapFrom
  :: HashMap ArkhamChaosToken ArkhamChaosTokenInternal
  -> HashMap ArkhamChaosToken ArkhamChaosTokenInternal
buildTokenMapFrom scenarioTokens = HashMap.union scenarioTokens defaultTokenMap

drawCard :: ArkhamGameData -> ArkhamGameData
drawCard g =
  let (drawn, deck') = splitAt 1 (g ^. player . deck)
  in g & player . hand %~ (++ drawn) & player . deck .~ deck'

defaultUpdateObjectives
  :: MonadIO m => Lockable ArkhamGame -> m (Lockable ArkhamGame)
defaultUpdateObjectives = runIgnoreLockedM $ \g ->
  let
    Just actCard = lookupAct . topOfStackCardCode <$> g ^. stacks . at "Act"
    Just agendaCard =
      lookupAgenda . topOfStackCardCode <$> g ^. stacks . at "Agenda"
  in pure g

defaultMythosPhase :: ArkhamMythosPhaseInternal
defaultMythosPhase = ArkhamMythosPhaseInternal
  { mythosPhaseOnEnter = pure
  , mythosPhaseAddDoom = runLockedM AddDoom
    $ \g -> pure . Unlocked $ g & stacks . at "Agenda" . _Just . doom +~ 1
  , mythosPhaseCheckAdvance = pure
  , mythosPhaseDrawEncounter = runOnlyUnlockedM $ \g -> do
    let (card : deck') = g ^. encounterDeck
    Unlocked
      <$> (g & encounterDeck .~ deck' & traverseOf
            (currentData . gameState)
            (aeiResolve
              (toInternalEncounterCard card)
              (g ^. player . investigator)
            )
          )
  , mythosPhaseOnExit = pure
  }

defaultInvestigationPhase :: ArkhamInvestigationPhaseInternal
defaultInvestigationPhase = ArkhamInvestigationPhaseInternal
  { investigationPhaseOnEnter = pure
  , investigationPhaseTakeActions = runLockedM InvestigationTakeActions $ \g ->
    if g ^. player . endedTurn
      then pure $ Unlocked g
      else pure $ addLock InvestigationTakeActions g
  , investigationPhaseOnExit = pure
  }

defaultEnemyPhase :: ArkhamEnemyPhaseInternal
defaultEnemyPhase = ArkhamEnemyPhaseInternal
  { enemyPhaseOnEnter = pure
  , enemyPhaseResolveHunters = pure
  , enemyPhaseResolveEnemies = pure
  , enemyPhaseOnExit = pure
  }

defaultUpkeepPhase :: ArkhamUpkeepPhaseInternal
defaultUpkeepPhase = ArkhamUpkeepPhaseInternal
  { upkeepPhaseOnEnter = pure
  , upkeepPhaseResetActions = runLockedM UpkeepResetActions $ \g ->
    pure . Unlocked $ g & player . actions .~ 3 & player . endedTurn .~ False
  , upkeepPhaseReadyExhausted = pure
  , upkeepPhaseDrawCardsAndGainResources =
    runLockedM DrawAndGainResources $ \g ->
      pure . Unlocked $ g & currentData %~ drawCard & player . resources +~ 1
  , upkeepPhaseCheckHandSize = pure
  , upkeepPhaseOnExit = pure
  }

defaultScenarioRun :: MonadIO m => ArkhamGame -> m ArkhamGame
defaultScenarioRun g = do
  result <- firstPass
  if isLocked result
    then pure (withoutLock result)
    else withoutLock <$> go result
 where
  firstPass = go (buildLock g)
  scenario' = toInternalScenario g
  ArkhamMythosPhaseInternal {..} = scenarioMythosPhase scenario'
  ArkhamInvestigationPhaseInternal {..} = scenarioInvestigationPhase scenario'
  ArkhamEnemyPhaseInternal {..} = scenarioEnemyPhase scenario'
  ArkhamUpkeepPhaseInternal {..} = scenarioUpkeepPhase scenario'
  go =
    scenarioUpdateObjectives scenario'
      >=> mythosPhaseOnEnter
      >=> mythosPhaseAddDoom
      >=> mythosPhaseCheckAdvance
      >=> mythosPhaseDrawEncounter
      >=> mythosPhaseOnExit
      >=> investigationPhaseOnEnter
      >=> investigationPhaseTakeActions
      >=> investigationPhaseOnExit
      >=> enemyPhaseOnEnter
      >=> enemyPhaseResolveHunters
      >=> enemyPhaseResolveEnemies
      >=> enemyPhaseOnExit
      >=> upkeepPhaseOnEnter
      >=> upkeepPhaseResetActions
      >=> upkeepPhaseReadyExhausted
      >=> upkeepPhaseDrawCardsAndGainResources
      >=> upkeepPhaseCheckHandSize
      >=> upkeepPhaseOnExit

theGathering :: ArkhamDifficulty -> ArkhamScenarioInternal
theGathering difficulty' = ArkhamScenarioInternal
  { scenarioName = "The Gathering"
  , scenarioSetup = theGatheringSetup
  , scenarioUpdateObjectives = defaultUpdateObjectives
  , scenarioMythosPhase = defaultMythosPhase
  , scenarioInvestigationPhase = defaultInvestigationPhase
  , scenarioEnemyPhase = defaultEnemyPhase
  , scenarioUpkeepPhase = defaultUpkeepPhase
  , scenarioRun = defaultScenarioRun
  , tokenMap = buildTokenMapFrom $ HashMap.fromList
    [ (Skull, theGatheringSkullToken difficulty')
    , (Cultist, theGatheringCultistToken difficulty')
    , (Tablet, theGatheringTabletToken difficulty')
    ]
  }

isEasyStandard :: ArkhamDifficulty -> Bool
isEasyStandard difficulty' =
  difficulty' == ArkhamEasy || difficulty' == ArkhamStandard

reveal :: ArkhamGameState -> ArkhamLocation -> ArkhamLocation
reveal g l = aliOnReveal (toLocationInternal l) g l

theGatheringSetup :: MonadRandom m => ArkhamGameState -> m ArkhamGameState
theGatheringSetup game = do
  agenda <- theGatheringAgenda
  act <- theGatheringAct
  let stacks' = HashMap.fromList [("Agenda", agenda), ("Act", act)]
  pure $ game & locations .~ locations' & stacks .~ stacks'
 where
  investigators = [game ^. player . investigator]
  locations' = HashMap.fromList
    [ ( alCardCode study
      , reveal game $ study { alInvestigators = investigators }
      )
    ]

theGatheringAgenda :: MonadRandom m => m ArkhamStack
theGatheringAgenda = pure $ AgendaStack $ NE.fromList
  [ ArkhamAgenda
    (ArkhamCardCode "01105")
    "https://arkhamdb.com/bundles/cards/01105.jpg"
    0
  , ArkhamAgenda
    (ArkhamCardCode "01106")
    "https://arkhamdb.com/bundles/cards/01106.jpg"
    0
  , ArkhamAgenda
    (ArkhamCardCode "01107")
    "https://arkhamdb.com/bundles/cards/01107.jpg"
    0
  ]

theGatheringAct :: MonadRandom m => m ArkhamStack
theGatheringAct = pure $ ActStack $ NE.fromList
  [ ArkhamAct
    (ArkhamCardCode "01108")
    "https://arkhamdb.com/bundles/cards/01108.jpg"
  , ArkhamAct
    (ArkhamCardCode "01109")
    "https://arkhamdb.com/bundles/cards/01109.jpg"
  , ArkhamAct
    (ArkhamCardCode "01110")
    "https://arkhamdb.com/bundles/cards/01110.jpg"
  ]

unrevealedLocation :: ArkhamLocation
unrevealedLocation = ArkhamLocation
  { alName = error "Missing location name"
  , alCardCode = error "Missing card code"
  , alLocationSymbol = Nothing
  , alConnectedLocationSymbols = []
  , alShroud = 0
  , alImage = error "Missing card image"
  , alInvestigators = []
  , alEnemies = []
  , alClues = 0
  , alDoom = 0
  , alStatus = Unrevealed
  }

study :: ArkhamLocation
study = unrevealedLocation
  { alName = "Study"
  , alCardCode = ArkhamCardCode "01111"
  , alLocationSymbol = Just Circle
  , alShroud = 2
  , alImage = "https://arkhamdb.com/bundles/cards/01111.png"
  }

theGatheringSkullToken :: ArkhamDifficulty -> ArkhamChaosTokenInternal
theGatheringSkullToken difficulty' = if isEasyStandard difficulty'
  then (token Skull)
    { tokenToResult = \g i -> Modifier . countTraitMatch Ghoul $ locationFor i g
    }
  else (token Skull)
    { tokenToResult = modifier (-2)
    , tokenOnFail = \_ _ -> error "TODO: Draw a ghoul"
    }

theGatheringCultistToken :: ArkhamDifficulty -> ArkhamChaosTokenInternal
theGatheringCultistToken difficulty' = if isEasyStandard difficulty'
  then (token Cultist)
    { tokenToResult = modifier (-1)
    , tokenOnFail = \g _ -> g & player . sanityDamage +~ 1
    }
  else (token Cultist)
    { tokenOnReveal = \_ _ -> error "TODO: Reveal another"
    , tokenOnFail = \g _ -> g & player . sanityDamage +~ 2
    }

theGatheringTabletToken :: ArkhamDifficulty -> ArkhamChaosTokenInternal
theGatheringTabletToken difficulty' = if isEasyStandard difficulty'
  then (token Tablet)
    { tokenToResult = modifier (-2)
    , tokenOnReveal = \g i -> if countTraitMatch Ghoul (locationFor i g) > 0
      then g & player . healthDamage +~ 1
      else g
    }
  else (token Tablet)
    { tokenToResult = modifier (-4)
    , tokenOnReveal = \g i -> if countTraitMatch Ghoul (locationFor i g) > 0
      then g & player . healthDamage +~ 1 & player . sanityDamage +~ 1
      else g
    }
