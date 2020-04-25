{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Handler.Api.Games where

import Arkham.Types
import Import
import Json

data GameJson = GameJson
  { gameId :: Text
  , gameCycle :: Maybe ArkhamCycle
  , gameScenario :: ArkhamScenario
  , gameInvestigators :: [ArkhamInvestigator]
  , gameActions :: [ArkhamAction]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "game") GameJson

rolandBanks :: ArkhamInvestigator
rolandBanks = ArkhamInvestigator
  { arkhamInvestigatorName = "Roland Banks"
  , arkhamInvestigatorWillpower = 3
  , arkhamInvestigatorIntellect = 3
  , arkhamInvestigatorCombat = 4
  , arkhamInvestigatorAgility = 2
  , arkhamInvestigatorHealth = 9
  , arkhamInvestigatorSanity = 5
  , arkhamInvestigatorFrontImageUrl = "https://arkhamdb.com/bundles/cards/01001.png"
  , arkhamInvestigatorBackImageUrl = "https://arkhamdb.com/bundles/cards/01001b.png"
  }

getApiV1ArkhamGameR :: ArkhamHorrorGameId -> Handler GameJson
getApiV1ArkhamGameR _ = do
  cycle <- liftIO $ decodeFileStrict' "data/arkham/cycles/nightOfTheZealot.json"
  pure $ GameJson "1" cycle scenario [rolandBanks] [ArkhamActionRevealLocation 0]
 where
  scenario = ArkhamScenario
    "The Gathering"
    [ ArkhamStackAgenda $ ArkhamAgenda $ ArkhamAgendaCardSideA $ ArkhamAgendaCardSideAData
      { arkhamAgendaCardSideADataName = ""
      , arkhamAgendaCardSideADataImageUrl = "https://arkhamdb.com/bundles/cards/01105.jpg"
      }
    , ArkhamStackAct $ ArkhamAct $ ArkhamActCardSideA $ ArkhamActCardSideAData
      { arkhamActCardSideADataName = ""
      , arkhamActCardSideADataImageUrl = "https://arkhamdb.com/bundles/cards/01108.jpg"
      }
    ]
    [ ArkhamLocationUnrevealed $
        ArkhamLocationUnrevealedData
          { arkhamLocationUnrevealedDataId = "1"
          , arkhamLocationUnrevealedDataName = "Study"
          , arkhamLocationUnrevealedDataSymbol = ArkhamLocationSymbolCircle
          , arkhamLocationUnrevealedDataImageUrl = "https://arkhamdb.com/bundles/cards/01111b.png"
          }
    ]
