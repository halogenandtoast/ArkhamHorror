module Arkham.Game.Settings where

import Arkham.Ai.Orphans ()
import Arkham.Ai.State (AiPlayerState)
import Arkham.Id (PlayerId)
import Arkham.Prelude
import Arkham.UltimatumsAndBoons.Types
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
  , settingsUltimatumsAndBoons :: Set UltimatumOrBoon
  -- ^ Variant rules selected at game creation; permanent for the campaign or
  -- standalone scenario per the FAQ, so nothing mutates this after creation.
  , settingsUltimatumsAndBoonsEnabled :: Bool
  -- ^ Runtime kill switch: when False, selected entries behave as if nothing
  -- were selected. Every effect hook reads through 'activeUltimatumsAndBoons'.
  }
  deriving stock (Eq, Show, Generic, Data)

-- | The single gate every Ultimatum/Boon hook must go through.
activeUltimatumsAndBoons :: Settings -> Set UltimatumOrBoon
activeUltimatumsAndBoons settings
  | settingsUltimatumsAndBoonsEnabled settings = settingsUltimatumsAndBoons settings
  | otherwise = mempty

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
    , settingsUltimatumsAndBoons = mempty
    , settingsUltimatumsAndBoonsEnabled = True
    }

instance ToJSON Settings where
  toJSON settings = object
    [ "settingsAbilitiesCannotReactToThemselves" .= settingsAbilitiesCannotReactToThemselves settings
    , "settingsAsIfRuling" .= settingsAsIfRuling settings
    , "settingsStrictAsIfAt" .= settingsStrictAsIfAt settings -- legacy/client compatibility
    , "aiPlayers" .= settingsAiPlayers settings
    , "settingsUltimatumsAndBoons" .= settingsUltimatumsAndBoons settings
    , "settingsUltimatumsAndBoonsEnabled" .= settingsUltimatumsAndBoonsEnabled settings
    ]

instance FromJSON Settings where
  parseJSON = withObject "Settings" \o -> do
    abilitiesCannotReactToThemselves <-
      o .:? "settingsAbilitiesCannotReactToThemselves" .!= defaultSettings.settingsAbilitiesCannotReactToThemselves
    legacyStrictAsIfAt <- o .:? "settingsStrictAsIfAt"
    asIfRuling <-
      o .:? "settingsAsIfRuling" .!= maybe defaultSettings.settingsAsIfRuling asIfRulingFromStrictAsIfAt legacyStrictAsIfAt
    aiPlayers <- o .:? "aiPlayers" .!= mempty
    ultimatumsAndBoons <- o .:? "settingsUltimatumsAndBoons" .!= mempty
    ultimatumsAndBoonsEnabled <- o .:? "settingsUltimatumsAndBoonsEnabled" .!= True
    pure
      Settings
        { settingsAbilitiesCannotReactToThemselves = abilitiesCannotReactToThemselves
        , settingsAsIfRuling = asIfRuling
        , settingsAiPlayers = aiPlayers
        , settingsUltimatumsAndBoons = ultimatumsAndBoons
        , settingsUltimatumsAndBoonsEnabled = ultimatumsAndBoonsEnabled
        }
