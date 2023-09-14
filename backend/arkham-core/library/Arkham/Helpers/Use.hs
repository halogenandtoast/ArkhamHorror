module Arkham.Helpers.Use where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue

toStartingUses :: HasGame m => Uses GameValue -> m (Uses Int)
toStartingUses (Uses uType gameValue) = do
  value <- getPlayerCountValue gameValue
  pure $ Uses uType value
toStartingUses (UsesWithLimit uType gameValue limitValue) = do
  value <- getPlayerCountValue gameValue
  limit <- getPlayerCountValue limitValue
  pure $ UsesWithLimit uType value limit
toStartingUses NoUses = pure NoUses

startingUseCountFor :: HasGame m => UseType -> Uses GameValue -> m Int
startingUseCountFor uType uses = do
  u' <- toStartingUses uses
  pure $ if useType u' == Just uType then useCount u' else 0

hasUsesFor :: UseType -> Uses Int -> Bool
hasUsesFor uType uses = useType uses == Just uType && useCount uses > 0
