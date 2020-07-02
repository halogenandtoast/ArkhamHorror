{-# LANGUAGE NamedFieldPuns #-}
module Arkham.Api.Handler.Acts
  ( postApiV1ArkhamGameProgressActR
  )
where

import Arkham.Internal.Act
import Arkham.Internal.Scenario
import Arkham.Internal.Types
import Arkham.Types
import Arkham.Types.Card
import Arkham.Types.Game
import Arkham.Util
import Data.Aeson
import Data.Aeson.Casing
import Import
import Lens.Micro

newtype ProgressActPost = ProgressActPost { papActCardCode :: ArkhamCardCode }
  deriving stock (Generic)

instance FromJSON ProgressActPost where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

postApiV1ArkhamGameProgressActR :: ArkhamGameId -> Handler ArkhamGameData
postApiV1ArkhamGameProgressActR gameId = do
  ProgressActPost { papActCardCode } <- requireCheckJsonBody
  -- find act with correct top card
  game <- runDB $ get404 gameId
  let
    scenario' = toInternalScenario game
    act = scenarioFindAct scenario' papActCardCode game
    internalAct = toInternalAct act
  -- TODO: should we introduct validation
  if actCanProgress internalAct (game ^. currentData . gameState)
    then
      traverseOf (currentData . gameState) (actOnProgress internalAct) game
      >>= runDB
      . updateGame gameId
    else sendResponseStatus badRequest400
      $ object ["error" .= ("act can not be progressed" :: Text)]
