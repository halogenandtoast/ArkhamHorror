module AH3e.Content.Scenarios (ScenarioInfo (..), scenarioCatalog) where

import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Ids

data ScenarioInfo = ScenarioInfo {code :: ScenarioCode, name :: Text, expansion :: Expansion}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

scenarioCatalog :: [ScenarioInfo]
scenarioCatalog =
  [ ScenarioInfo "approach-of-azathoth" "Approach of Azathoth" CoreSet
  , ScenarioInfo "feast-of-umordhoth" "Feast of Umôrdhoth" CoreSet
  , ScenarioInfo "veil-of-twilight" "Veil of Twilight" CoreSet
  , ScenarioInfo "echoes-of-the-deep" "Echoes of the Deep" CoreSet
  , ScenarioInfo "shots-in-the-dark" "Shots in the Dark" DeadOfNight
  , ScenarioInfo "silence-of-tsathoggua" "Silence of Tsathoggua" DeadOfNight
  , ScenarioInfo "the-pale-lantern" "The Pale Lantern" UnderDarkWaves
  , ScenarioInfo "dreams-of-rlyeh" "Dreams of R'lyeh" UnderDarkWaves
  , ScenarioInfo "ithaquas-children" "Ithaqua's Children" UnderDarkWaves
  , ScenarioInfo "tyrants-of-ruin" "Tyrants of Ruin" UnderDarkWaves
  , ScenarioInfo "the-dead-cry-out" "The Dead Cry Out" SecretsOfTheOrder
  , ScenarioInfo "bound-to-serve" "Bound to Serve" SecretsOfTheOrder
  , ScenarioInfo "the-key-and-the-gate" "The Key and the Gate" SecretsOfTheOrder
  ]
