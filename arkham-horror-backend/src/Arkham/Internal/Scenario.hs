module Arkham.Internal.Scenario where

import Arkham.Constructors
import Arkham.Internal.ChaosToken
import Arkham.Internal.Investigator
import Arkham.Internal.Types
import Arkham.Types
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
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

defaultMythosPhase :: ArkhamMythosPhaseInternal
defaultMythosPhase = ArkhamMythosPhaseInternal
  { mythosPhaseOnEnter = id
  , mythosPhaseAddDoom = runLocked "addDoom"
    $ \g -> Unlocked $ g & stacks . at "Act" . _Just . doom +~ 1
  , mythosPhaseCheckAdvance = id
  -- , mythosCheckAdvance = \g -> actCheckAdvance g $ toActInternal (fromJustNote "Unknown act deck" $ g ^. stacks . at "Act")
  , mythosPhaseDrawEncounter = id
  , mythosPhaseOnExit = id
  }

defaultInvestigationPhase :: ArkhamInvestigationPhaseInternal
defaultInvestigationPhase = ArkhamInvestigationPhaseInternal
  { investigationPhaseOnEnter = id
  , investigationPhaseTakeActions =
    runLocked "investigationTakeActions" $ \g -> if g ^. player . endedTurn
      then Unlocked g
      else addLock "investigationTakeActions" g
  , investigationPhaseOnExit = id
  }

defaultEnemyPhase :: ArkhamEnemyPhaseInternal
defaultEnemyPhase = ArkhamEnemyPhaseInternal
  { enemyPhaseOnEnter = id
  , enemyPhaseResolveHunters = id
  , enemyPhaseResolveEnemies = id
  , enemyPhaseOnExit = id
  }

defaultUpkeepPhase :: ArkhamUpkeepPhaseInternal
defaultUpkeepPhase = ArkhamUpkeepPhaseInternal
  { upkeepPhaseOnEnter = id
  , upkeepPhaseResetActions = runLocked "upkeepResetActions"
    $ \g -> Unlocked $ g & player . actions .~ 3 & player . endedTurn .~ False
  , upkeepPhaseReadyExhausted = id
  , upkeepPhaseDrawCardsAndGainResources = runLocked "drawAndGainResource"
    $ \g -> Unlocked $ g & currentData %~ drawCard & player . resources +~ 1
  , upkeepPhaseCheckHandSize = id
  , upkeepPhaseOnExit = id
  }

defaultScenarioRun :: ArkhamGame -> ArkhamGame
defaultScenarioRun g = withoutLock
  $ if isLocked firstPass then firstPass else go firstPass
 where
  firstPass = go (buildLock g)
  scenario' = toInternalScenario g
  ArkhamMythosPhaseInternal {..} = scenarioMythosPhase scenario'
  ArkhamInvestigationPhaseInternal {..} = scenarioInvestigationPhase scenario'
  ArkhamEnemyPhaseInternal {..} = scenarioEnemyPhase scenario'
  ArkhamUpkeepPhaseInternal {..} = scenarioUpkeepPhase scenario'
  go g' =
    g'
      & mythosPhaseOnEnter
      & mythosPhaseAddDoom
      & mythosPhaseCheckAdvance
      & mythosPhaseDrawEncounter
      & mythosPhaseOnExit
      & investigationPhaseOnEnter
      & investigationPhaseTakeActions
      & investigationPhaseOnExit
      & enemyPhaseOnEnter
      & enemyPhaseResolveHunters
      & enemyPhaseResolveEnemies
      & enemyPhaseOnExit
      & upkeepPhaseOnEnter
      & upkeepPhaseResetActions
      & upkeepPhaseReadyExhausted
      & upkeepPhaseDrawCardsAndGainResources
      & upkeepPhaseCheckHandSize
      & upkeepPhaseOnExit

theGathering :: ArkhamDifficulty -> ArkhamScenarioInternal
theGathering difficulty' = ArkhamScenarioInternal
  { scenarioName = "The Gathering"
  , scenarioSetup = theGatheringSetup
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

theGatheringSetup :: ArkhamGameState -> ArkhamGameState
theGatheringSetup game = game & locations .~ locations' & stacks .~ stacks'
  where
    investigators = [game ^. player . investigator]
    locations' = HashMap.fromList $ [(alCardCode study, study { alInvestigators = investigators })]
    stacks' = HashMap.fromList $ [("Agenda", agenda), ("Act", act)]
    agenda = AgendaStack $ ArkhamAgenda
      (ArkhamCardCode "01105")
      "https://arkhamdb.com/bundles/cards/01105.jpg"
    act = ActStack $ ArkhamAct
      (ArkhamCardCode "01108")
      "https://arkhamdb.com/bundles/cards/01108.jpg"
      0


study :: ArkhamLocation
study = ArkhamLocation
  "Study"
  (ArkhamCardCode "01111")
  []
  2
  "https://arkhamdb.com/bundles/cards/01111.png"
  []
  2
  0
  Revealed

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
