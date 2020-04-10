{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Api.Arkham.Scenarios where

import Import
import qualified Data.Map.Strict as Map
import Data.Aeson.Types (ToJSONKey(..))

newtype Scenario = Scenario { getScenario :: Text }
  deriving newtype (ToJSON)

newtype Cycle = Cycle { getCycle :: Text }
  deriving newtype (Eq, Ord, ToJSONKey)

getApiV1ArkhamScenariosR :: Handler (Map Cycle [Scenario])
getApiV1ArkhamScenariosR = pure $ Map.fromList
  [ (Cycle "Night of the Zealot"
    , [ Scenario "The Gathering"
      , Scenario "The Midnight Masks"
      , Scenario "The Devourer Below"
      ]
    )
  , (Cycle "The Dunwich Legacy"
    , [ Scenario "Extracurricular Activity"
      , Scenario "The House Always Wins"
      , Scenario "The Miskatonic Museum"
      , Scenario "The Essex County Express"
      , Scenario "Blood on the Altar"
      , Scenario "Undimensioned and Unseen"
      , Scenario "Where Doom Awaits"
      , Scenario "Lost in Time and Space"
      ]
    )
  ]
