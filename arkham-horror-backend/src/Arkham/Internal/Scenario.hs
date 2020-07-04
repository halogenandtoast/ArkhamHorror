{-# LANGUAGE NamedFieldPuns #-}
module Arkham.Internal.Scenario
  ( toInternalScenario
  , drawCard
  )
where

import Arkham.Constructors
import Arkham.Entity.ArkhamGame
import Arkham.Internal.Act
import Arkham.Internal.ChaosToken
import Arkham.Internal.EncounterCard
import Arkham.Internal.Location
import Arkham.Internal.Types
import Arkham.Types
import Arkham.Types.Act
import Arkham.Types.Card
import Arkham.Types.ChaosToken
import Arkham.Types.Difficulty
import Arkham.Types.Enemy
import Arkham.Types.Game
import Arkham.Types.GameState
import Arkham.Types.Location
import Arkham.Types.Player
import Arkham.Types.Scenario
import Arkham.Types.Trait
import Base.Lock
import ClassyPrelude
import Control.Monad.Random
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NE
import Lens.Micro
import Lens.Micro.Platform ()
import Safe hiding (at)

locationEnemies :: HasEnemies a => a -> ArkhamLocation -> [ArkhamEnemy]
locationEnemies g l = map (fromJustNote "could not lookup enemy" . flip HashMap.lookup (g ^. enemies)) (l ^. enemyIds)

countTraitMatch :: HasTraits a => ArkhamTrait -> [a] -> Int
countTraitMatch trait' cards' = length . filter (trait' `elem`) $ cards' ^.. each . traits

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
  let (drawn, deck') = splitAt 1 (g ^. activePlayer . deck)
  in g & activePlayer . hand %~ (++ drawn) & activePlayer . deck .~ deck'

defaultUpdateObjectives
  :: MonadIO m => Lockable ArkhamGame -> m (Lockable ArkhamGame)
defaultUpdateObjectives = runIgnoreLockedM $ \g ->
  let
    actCard = fromJustNote "Could not find Act" (g ^? topActCardLens)
    ArkhamActInternal { actCanProgress } = toInternalAct actCard
    actCard' = actCard
      { aactCanProgress = actCanProgress (g ^. currentData . gameState)
      }
  in pure $ g & topActCardLens .~ actCard'
  where topActCardLens = stacks . ix "Act" . _ActStack . _TopOfStack

defaultUpdateAccessibleLocationsOnPlayers
  :: MonadIO m => Lockable ArkhamGame -> m (Lockable ArkhamGame)
defaultUpdateAccessibleLocationsOnPlayers = runIgnoreLockedM
  $ \g -> pure $ g & players . mapped %~ updateAccessibleLocations g

updateAccessibleLocations :: ArkhamGame -> ArkhamPlayer -> ArkhamPlayer
updateAccessibleLocations g p = p
  { _accessibleLocations = map alCardCode accessibleLocations
  }
 where
  matchingSymbols =
    alConnectedLocationSymbols $ locationFor p (g ^. currentData . gameState)
  currentLocations = HashMap.elems (g ^. locations)
  accessibleLocations =
    filter
        (\l -> aliCanEnter (toLocationInternal l) (g ^. currentData . gameState)
        )
      $ filter
          (maybe False (`elem` matchingSymbols) . alLocationSymbol)
          currentLocations
    -- HashMap CardCode ArkhamLocation

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
            (aeiResolve (toInternalEncounterCard card) (g ^. activePlayer))
          )
  , mythosPhaseOnExit = pure
  }

defaultInvestigationPhase :: ArkhamInvestigationPhaseInternal
defaultInvestigationPhase = ArkhamInvestigationPhaseInternal
  { investigationPhaseOnEnter = pure
  , investigationPhaseTakeActions = runLockedM InvestigationTakeActions $ \g ->
    if g ^. activePlayer . endedTurn
      then pure $ Unlocked g
      else pure $ addLock (pure InvestigationTakeActions) g
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
    pure
      . Unlocked
      $ g
      & activePlayer
      . actions
      .~ 3
      & activePlayer
      . endedTurn
      .~ False
  , upkeepPhaseReadyExhausted = pure
  , upkeepPhaseDrawCardsAndGainResources =
    runLockedM DrawAndGainResources $ \g ->
      pure
        . Unlocked
        $ g
        & currentData
        %~ drawCard
        & activePlayer
        . resources
        +~ 1
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
      >=> scenarioUpdateAccessibleLocationsOnPlayers scenario'
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

-- TODO: validate card code
defaultScenarioFindAct :: ArkhamCardCode -> ArkhamGame -> ArkhamAct
defaultScenarioFindAct code' game' =
  fromJustNote ("Could not find act in scenario with id " <> tcode)
    $ game'
    ^? stacks
    . ix "Act"
    . _ActStack
    . _TopOfStack
  where tcode = unpack $ unArkhamCardCode code'

theGathering :: ArkhamDifficulty -> ArkhamScenarioInternal
theGathering difficulty' = ArkhamScenarioInternal
  { scenarioName = "The Gathering"
  , scenarioSetup = theGatheringSetup
  , scenarioUpdateObjectives = defaultUpdateObjectives
  , scenarioUpdateAccessibleLocationsOnPlayers =
    defaultUpdateAccessibleLocationsOnPlayers
  , scenarioMythosPhase = defaultMythosPhase
  , scenarioInvestigationPhase = defaultInvestigationPhase
  , scenarioEnemyPhase = defaultEnemyPhase
  , scenarioUpkeepPhase = defaultUpkeepPhase
  , scenarioRun = defaultScenarioRun
  , scenarioFindAct = defaultScenarioFindAct
  , scenarioTokenMap = buildTokenMapFrom $ HashMap.fromList
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
  investigators' = HashMap.elems (agsUsers game)
  locations' = HashMap.fromList
    [(alCardCode study, reveal game $ study & investigators .~ investigators')]

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
    False
  , ArkhamAct
    (ArkhamCardCode "01109")
    "https://arkhamdb.com/bundles/cards/01109.jpg"
    False
  , ArkhamAct
    (ArkhamCardCode "01110")
    "https://arkhamdb.com/bundles/cards/01110.jpg"
    False
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
    { tokenToResult = \g p -> Modifier . countTraitMatch Ghoul . locationEnemies g $ locationFor p g
    }
  else (token Skull)
    { tokenToResult = modifier (-2)
    , tokenOnFail = \_ _ -> error "TODO: Draw a ghoul"
    }

theGatheringCultistToken :: ArkhamDifficulty -> ArkhamChaosTokenInternal
theGatheringCultistToken difficulty' = if isEasyStandard difficulty'
  then (token Cultist)
    { tokenToResult = modifier (-1)
    , tokenOnFail = \g _ -> g & activePlayer . sanityDamage +~ 1
    }
  else (token Cultist)
    { tokenOnReveal = \_ _ -> error "TODO: Reveal another"
    , tokenOnFail = \g _ -> g & activePlayer . sanityDamage +~ 2
    }

theGatheringTabletToken :: ArkhamDifficulty -> ArkhamChaosTokenInternal
theGatheringTabletToken difficulty' = if isEasyStandard difficulty'
  then (token Tablet)
    { tokenToResult = modifier (-2)
    , tokenOnReveal = \g p -> if countTraitMatch Ghoul (locationEnemies g (locationFor p g)) > 0
      then g & activePlayer . healthDamage +~ 1
      else g
    }
  else (token Tablet)
    { tokenToResult = modifier (-4)
    , tokenOnReveal = \g p -> if countTraitMatch Ghoul (locationEnemies g (locationFor p g)) > 0
      then
        g & activePlayer . healthDamage +~ 1 & activePlayer . sanityDamage +~ 1
      else g
    }
