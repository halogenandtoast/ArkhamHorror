module Arkham.Game.Settings where

import Arkham.Prelude

data Settings = Settings
  { settingsAbilitiesCannotReactToThemselves :: Bool -- Grotesque Statue FAQ (September 2023)
  }
  deriving stock (Eq, Show, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

defaultSettings :: Settings
defaultSettings =
  Settings
    { settingsAbilitiesCannotReactToThemselves = True
    }
