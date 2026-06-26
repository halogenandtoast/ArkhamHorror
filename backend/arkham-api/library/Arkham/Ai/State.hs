module Arkham.Ai.State (
  AiPlayerState (..),
  defaultAiPlayerState,
) where

import Arkham.Ai.Focus (Focus)
import Arkham.Card.CardCode (CardCode)
import Arkham.Prelude
import Arkham.Target (Target)

-- | Per-player AI configuration, serialized into the game blob.
--
-- 'aiInvestigatorCode' names the tag profile to drive behaviour from (an
-- investigator card code such as @"01001"@); it is independent of which
-- investigator the seat actually controls so a profile can be reused.
data AiPlayerState = AiPlayerState
  { aiEnabled :: Bool
  , aiInvestigatorCode :: CardCode
  , aiFocusOverride :: Maybe Focus
  , aiPriorities :: [Target]
  , aiResponseDelayMs :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON AiPlayerState where
  toJSON s =
    object
      [ "aiEnabled" .= aiEnabled s
      , "aiInvestigatorCode" .= aiInvestigatorCode s
      , "aiFocusOverride" .= aiFocusOverride s
      , "aiPriorities" .= aiPriorities s
      , "aiResponseDelayMs" .= aiResponseDelayMs s
      ]

instance FromJSON AiPlayerState where
  parseJSON = withObject "AiPlayerState" \o -> do
    aiEnabled <- o .:? "aiEnabled" .!= True
    aiInvestigatorCode <- o .: "aiInvestigatorCode"
    aiFocusOverride <- o .:? "aiFocusOverride"
    aiPriorities <- o .:? "aiPriorities" .!= []
    aiResponseDelayMs <- o .:? "aiResponseDelayMs" .!= 1500
    pure AiPlayerState {..}

defaultAiPlayerState :: CardCode -> AiPlayerState
defaultAiPlayerState code =
  AiPlayerState
    { aiEnabled = True
    , aiInvestigatorCode = code
    , aiFocusOverride = Nothing
    , aiPriorities = []
    , aiResponseDelayMs = 1500
    }
