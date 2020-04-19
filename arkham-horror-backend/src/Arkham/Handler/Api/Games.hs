{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Handler.Api.Games where

import Import
import Json
import Arkham.Types

data GameJson = GameJson
  { gameCycle :: Maybe ArkhamCycle
  , gameScenario :: ArkhamScenario
  }
  deriving stock (Generic)
  deriving (ToJSON) via Codec (Drop "game") GameJson

getApiV1ArkhamGameR :: ArkhamHorrorGameId -> Handler GameJson
getApiV1ArkhamGameR _ = do
  cycle <- liftIO $ decodeFileStrict' "data/arkham/cycles/nightOfTheZealot.json"
  pure $ GameJson cycle scenario
  where
    scenario = ArkhamScenario "The Gathering"
      [ StackAgenda
          $ Agenda
          $ ArkhamCard
            (ArkhamCardFront "https://arkhamdb.com/bundles/cards/01105.jpg")
            (ArkhamCardBack "https://arkhamdb.com/bundles/cards/01105b.jpg")
      , StackAct
          $ Act
          $ ArkhamCard
            (ArkhamCardFront "https://arkhamdb.com/bundles/cards/01108.jpg")
            (ArkhamCardBack "https://arkhamdb.com/bundles/cards/01108b.jpg")
      ]
