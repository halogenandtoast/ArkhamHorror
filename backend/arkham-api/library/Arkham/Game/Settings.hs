module Arkham.Game.Settings where

import Arkham.Prelude

data Settings = Settings
  { settingsAbilitiesCannotReactToThemselves :: Bool -- Grotesque Statue FAQ (September 2023)
  , settingsStrictAsIfAt :: Bool -- "As If" ruling (Chapter 2): AsIfAt only applies during action resolution, not window triggers
  }
  deriving stock (Eq, Show, Generic, Data)
  deriving anyclass (ToJSON)

defaultSettings :: Settings
defaultSettings =
  Settings
    { settingsAbilitiesCannotReactToThemselves = True
    , settingsStrictAsIfAt = False
    }

instance FromJSON Settings where
  parseJSON = withObject "Settings" \o ->
    Settings
      <$> o .:? "settingsAbilitiesCannotReactToThemselves" .!= defaultSettings.settingsAbilitiesCannotReactToThemselves
      <*> o .:? "settingsStrictAsIfAt" .!= defaultSettings.settingsStrictAsIfAt
