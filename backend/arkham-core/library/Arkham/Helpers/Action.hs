module Arkham.Helpers.Action where

import Arkham.Prelude

import Arkham.Action.Additional
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Source

additionalActionCovers
  :: (Monad m, HasGame m)
  => Source
  -> Maybe Action
  -> AdditionalAction
  -> m Bool
additionalActionCovers source maction = \case
  TraitRestrictedAdditionalAction t -> member t <$> sourceTraits source
  ActionRestrictedAdditionalAction a -> pure $ maction == Just a
