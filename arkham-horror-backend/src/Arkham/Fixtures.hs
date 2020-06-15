module Arkham.Fixtures where

import Arkham.Types
import ClassyPrelude
import qualified Data.List.NonEmpty as NE

loadGameFixture :: Int -> IO ArkhamGame
loadGameFixture _ =  pure $ ArkhamGame NightOfTheZealot ScenarioOne gameState
 where
  gameState = ArkhamGameState player Investigation chaosTokens
  chaosTokens = NE.fromList [PlusOne, PlusOne, Zero, Zero, Zero, MinusOne, MinusOne, MinusOne, MinusTwo, MinusTwo, Skull, Skull, Cultist, Tablet, Fail, ElderSign]
  player = ArkhamPlayer rolandBanks 0 0 5 [machete] []
  machete = ArkhamCard (Just 3) "https://arkhamdb.com/bundles/cards/01020.png"
  rolandBanks = ArkhamInvestigator
    "Roland Banks"
    "https://arkhamdb.com/bundles/cards/01001.png"


