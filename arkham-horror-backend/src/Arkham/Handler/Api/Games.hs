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
  deriving (ToJSON) via Codec (Drop "game") GameJson

getApiV1ArkhamGameR :: ArkhamGameId -> Handler GameJson
getApiV1ArkhamGameR _ = do
  cycle <- liftIO $ decodeFileStrict' "data/arkham/cycles/nightOfTheZealot.json"
  scenario <-
    liftIO
    $ decodeFileStrict'
        "data/arkham/scenarios/nightOfTheZealot/theGathering.json"
    >>= maybe (throwString "parseFailed") pure
  rolandBanks <- runDB $ entityVal <$> getBy404
    (UniqueInvestigatorTitle "Roland Banks")
  pure
    $ GameJson "1" cycle scenario [rolandBanks] [ArkhamActionRevealLocation 0]
