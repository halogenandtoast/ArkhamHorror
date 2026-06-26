module Arkham.Game.Settings where

import Arkham.Ai.Orphans ()
import Arkham.Ai.State (AiPlayerState)
import Arkham.Id (PlayerId)
import Arkham.Prelude
import Control.Monad.Fail

data AsIfRuling
  = Chapter1AsIfRuling -- "As if" applies while checking/resolving nested windows.
  | Chapter2AsIfRuling -- "As if" applies during action resolution, not window triggers.
  deriving stock (Eq, Ord, Show, Generic, Data)

instance ToJSON AsIfRuling where
  toJSON = \case
    Chapter1AsIfRuling -> String "chapter1"
    Chapter2AsIfRuling -> String "chapter2"

instance FromJSON AsIfRuling where
  parseJSON = withText "AsIfRuling" \case
    "chapter1" -> pure Chapter1AsIfRuling
    "chapter2" -> pure Chapter2AsIfRuling
    "Chapter1AsIfRuling" -> pure Chapter1AsIfRuling
    "Chapter2AsIfRuling" -> pure Chapter2AsIfRuling
    other -> fail $ "unknown AsIfRuling: " <> unpack other

data Settings = Settings
  { settingsAbilitiesCannotReactToThemselves :: Bool -- Grotesque Statue FAQ (September 2023)
  , settingsAsIfRuling :: AsIfRuling
  , settingsAiPlayers :: Map PlayerId AiPlayerState
  -- ^ Per-seat AI configuration. Empty for ordinary human-only games; absent
  -- from older saves (defaults to 'mempty' on load).
  }
  deriving stock (Eq, Show, Generic, Data)

settingsStrictAsIfAt :: Settings -> Bool
settingsStrictAsIfAt = (== Chapter2AsIfRuling) . settingsAsIfRuling

asIfRulingFromStrictAsIfAt :: Bool -> AsIfRuling
asIfRulingFromStrictAsIfAt = \case
  False -> Chapter1AsIfRuling
  True -> Chapter2AsIfRuling

defaultAsIfRulingForCampaign :: Maybe Text -> AsIfRuling
defaultAsIfRulingForCampaign = \case
  Just cid | cid >= "11" -> Chapter2AsIfRuling
  _ -> Chapter1AsIfRuling

defaultSettings :: Settings
defaultSettings =
  Settings
    { settingsAbilitiesCannotReactToThemselves = True
    , settingsAsIfRuling = Chapter1AsIfRuling
    , settingsAiPlayers = mempty
    }

instance ToJSON Settings where
  toJSON settings = object
    [ "settingsAbilitiesCannotReactToThemselves" .= settingsAbilitiesCannotReactToThemselves settings
    , "settingsAsIfRuling" .= settingsAsIfRuling settings
    , "settingsStrictAsIfAt" .= settingsStrictAsIfAt settings -- legacy/client compatibility
    , "aiPlayers" .= settingsAiPlayers settings
    ]

instance FromJSON Settings where
  parseJSON = withObject "Settings" \o -> do
    abilitiesCannotReactToThemselves <-
      o .:? "settingsAbilitiesCannotReactToThemselves" .!= defaultSettings.settingsAbilitiesCannotReactToThemselves
    legacyStrictAsIfAt <- o .:? "settingsStrictAsIfAt"
    asIfRuling <-
      o .:? "settingsAsIfRuling" .!= maybe defaultSettings.settingsAsIfRuling asIfRulingFromStrictAsIfAt legacyStrictAsIfAt
    aiPlayers <- o .:? "aiPlayers" .!= mempty
    pure
      Settings
        { settingsAbilitiesCannotReactToThemselves = abilitiesCannotReactToThemselves
        , settingsAsIfRuling = asIfRuling
        , settingsAiPlayers = aiPlayers
        }
