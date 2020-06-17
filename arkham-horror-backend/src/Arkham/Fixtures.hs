module Arkham.Fixtures where

import Arkham.Types
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NE

loadGameFixture :: Int -> IO ArkhamGame
loadGameFixture _ = pure $ ArkhamGame 1 NightOfTheZealot theGathering gameState
 where
  theGathering = ArkhamScenario
    "The Gathering"
    "https://arkhamdb.com/bundles/cards/01104.jpg"
  gameState = ArkhamGameState
    player
    Investigation
    chaosTokens
    [RevealedLocation study]
    (HashMap.fromList
      [("Study", [LocationInvestigator rolandBanks, LocationClues 2])]
    )
    [agenda, act]
  agenda =
    AgendaStack $ ArkhamAgenda "https://arkhamdb.com/bundles/cards/01105.jpg"
  act = ActStack $ ArkhamAct "https://arkhamdb.com/bundles/cards/01108.jpg"
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
  study = ArkhamRevealedLocation
    "Study"
    "Study"
    []
    2
    "https://arkhamdb.com/bundles/cards/01111.png"
  player = ArkhamPlayer rolandBanks 0 0 5 0 [machete] []
  machete = PlayerCard $ ArkhamPlayerCard
    "Machete"
    (Just 3)
    "https://arkhamdb.com/bundles/cards/01020.png"
  rolandBanks = ArkhamInvestigator
    "Roland Banks"
    "https://arkhamdb.com/bundles/cards/01001.png"
    "/img/arkham/AHC01_1_portrait.jpg"
    (ArkhamSkillWillpower 3)
    (ArkhamSkillIntellect 3)
    (ArkhamSkillCombat 4)
    (ArkhamSkillAgility 2)
