module Arkham.Fixtures where

import Arkham.Types
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

loadGameFixture :: Int -> IO ArkhamGame
loadGameFixture _ = pure $ ArkhamGame 1 NightOfTheZealot theGathering fixtureGameState

theGathering :: ArkhamScenario
theGathering =
  ArkhamScenario "The Gathering" "https://arkhamdb.com/bundles/cards/01104.jpg"

fixtureGameState :: ArkhamGameState
fixtureGameState = ArkhamGameState
  player
  Investigation
  chaosTokens
  [RevealedLocation study]
  (HashMap.fromList
    [("Study", [LocationInvestigator rolandBanks, LocationClues 2])]
  )
  [agenda, act]
  ArkhamGameStateStepInvestigatorActionStep

agenda :: ArkhamStack
agenda =
  AgendaStack $ ArkhamAgenda "https://arkhamdb.com/bundles/cards/01105.jpg"

act :: ArkhamStack
act = ActStack $ ArkhamAct "https://arkhamdb.com/bundles/cards/01108.jpg"

chaosTokens :: NonEmpty ArkhamChaosToken
chaosTokens = NE.fromList
  [ PlusOne
  , PlusOne
  , Zero
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , Skull
  , Skull
  , Cultist
  , Tablet
  , AutoFail
  , ElderSign
  ]

study :: ArkhamRevealedLocation
study = ArkhamRevealedLocation
  "Study"
  "Study"
  []
  2
  "https://arkhamdb.com/bundles/cards/01111.png"

player :: ArkhamPlayer
player = ArkhamPlayer rolandBanks 0 0 5 0 [machete] []

machete :: ArkhamCard
machete = PlayerCard $ ArkhamPlayerCard
  "Machete"
  (Just 3)
  "https://arkhamdb.com/bundles/cards/01020.png"

rolandBanks :: ArkhamInvestigator
rolandBanks = ArkhamInvestigator
  "Roland Banks"
  "https://arkhamdb.com/bundles/cards/01001.png"
  "/img/arkham/AHC01_1_portrait.jpg"
  (ArkhamSkill 3)
  (ArkhamSkill 3)
  (ArkhamSkill 4)
  (ArkhamSkill 2)
