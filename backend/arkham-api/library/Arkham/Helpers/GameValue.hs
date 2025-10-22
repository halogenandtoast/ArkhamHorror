module Arkham.Helpers.GameValue (module Arkham.Helpers.GameValue, module X) where

import Arkham.Classes.HasGame
import Arkham.GameValue as X
import Arkham.Helpers.Query
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
import Arkham.Tracing

getPlayerCountValue :: (HasGame m, Tracing m) => GameValue -> m Int
getPlayerCountValue gameValue = fromGameValue gameValue <$> getPlayerCount

getGameValue :: (HasGame m, Tracing m) => GameValue -> m Int
getGameValue = getPlayerCountValue

perPlayer :: (HasGame m, Tracing m) => Int -> m Int
perPlayer = getPlayerCountValue . PerPlayer

gameValueMatches :: (HasGame m, Tracing m) => Int -> Matcher.ValueMatcher -> m Bool
gameValueMatches n = \case
  Matcher.AnyValue -> pure True
  Matcher.LessThan gv -> (n <) <$> getPlayerCountValue gv
  Matcher.GreaterThan gv -> (n >) <$> getPlayerCountValue gv
  Matcher.LessThanOrEqualTo gv -> (n <=) <$> getPlayerCountValue gv
  Matcher.GreaterThanOrEqualTo gv -> (n >=) <$> getPlayerCountValue gv
  Matcher.EqualTo gv -> (n ==) <$> getPlayerCountValue gv
  Matcher.Between l h -> do
    andM
      [ (n >=) <$> getPlayerCountValue l
      , (n <=) <$> getPlayerCountValue h
      ]
  Matcher.GameValueOneOf xs -> anyM (gameValueMatches n) xs
