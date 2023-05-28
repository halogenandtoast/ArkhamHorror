module Arkham.Helpers.Use where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Store

toStartingUses :: (HasGame m, Store m Card) => Uses GameValue -> m (Uses Int)
toStartingUses (Uses uType gameValue) = do
  value <- getPlayerCountValue gameValue
  pure $ Uses uType value
toStartingUses (UsesWithLimit uType gameValue limitValue) = do
  value <- getPlayerCountValue gameValue
  limit <- getPlayerCountValue limitValue
  pure $ UsesWithLimit uType value limit
toStartingUses NoUses = pure NoUses
