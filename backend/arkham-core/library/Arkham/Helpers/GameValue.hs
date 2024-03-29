module Arkham.Helpers.GameValue where

import Arkham.Classes.HasGame
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Prelude

getPlayerCountValue :: HasGame m => GameValue -> m Int
getPlayerCountValue gameValue = fromGameValue gameValue <$> getPlayerCount

getGameValue :: HasGame m => GameValue -> m Int
getGameValue = getPlayerCountValue

perPlayer :: HasGame m => Int -> m Int
perPlayer = getPlayerCountValue . PerPlayer
