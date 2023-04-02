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
toStartingUses NoUses = pure NoUses

