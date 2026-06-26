module Arkham.Ai.Helpers (
  getAiPlayerState,
  lookupAiPlayer,
  overAiPlayers,
  overAiSeat,
) where

import Arkham.Ai.State (AiPlayerState)
import Arkham.Classes.HasGame (HasGame, getGame)
import Arkham.Game.Base (Game (gameSettings))
import Arkham.Game.Settings (Settings (settingsAiPlayers))
import Arkham.Id (PlayerId)
import Arkham.Prelude
import Data.Map.Strict qualified as Map

-- | Pure lookup of a seat's AI configuration from the game's 'Settings'.
lookupAiPlayer :: PlayerId -> Settings -> Maybe AiPlayerState
lookupAiPlayer pid = Map.lookup pid . settingsAiPlayers

-- | The AI configuration for a seat, if it has been registered.
getAiPlayerState :: HasGame m => PlayerId -> m (Maybe AiPlayerState)
getAiPlayerState pid = lookupAiPlayer pid . gameSettings <$> getGame

-- | Modify the whole AI-seat map, writing the new 'Settings' back into the game.
overAiPlayers :: (Map PlayerId AiPlayerState -> Map PlayerId AiPlayerState) -> Game -> Game
overAiPlayers f g = g {gameSettings = s {settingsAiPlayers = f (settingsAiPlayers s)}}
 where
  s = gameSettings g

-- | Modify a single registered seat's 'AiPlayerState'. A no-op when the seat
-- is not registered.
overAiSeat :: PlayerId -> (AiPlayerState -> AiPlayerState) -> Game -> Game
overAiSeat pid f = overAiPlayers (Map.adjust f pid)
