module Arkham.Helpers.GameValue (module Arkham.Helpers.GameValue, module X) where

import Arkham.Classes.HasGame
import Arkham.GameValue as X
import Arkham.Helpers.Query
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude

getPlayerCountValue :: HasGame m => GameValue -> m Int
getPlayerCountValue gameValue = fromGameValue gameValue <$> getPlayerCount

getGameValue :: HasGame m => GameValue -> m Int
getGameValue = getPlayerCountValue

perPlayer :: HasGame m => Int -> m Int
perPlayer = getPlayerCountValue . PerPlayer

gameValueMatches :: HasGame m => Int -> Matcher.ValueMatcher -> m Bool
gameValueMatches n = \case
  Matcher.AnyValue -> pure True
  Matcher.LessThan gv -> (n <) <$> getPlayerCountValue gv
  Matcher.GreaterThan gv -> (n >) <$> getPlayerCountValue gv
  Matcher.LessThanOrEqualTo gv -> (n <=) <$> getPlayerCountValue gv
  Matcher.GreaterThanOrEqualTo gv -> (n >=) <$> getPlayerCountValue gv
  Matcher.EqualTo gv -> (n ==) <$> getPlayerCountValue gv
