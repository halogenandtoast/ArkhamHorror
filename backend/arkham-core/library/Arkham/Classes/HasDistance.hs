module Arkham.Classes.HasDistance where

import Arkham.Prelude

import Arkham.Distance
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id

class HasDistance a where
  getDistance' :: HasGame m => a -> LocationId -> LocationId -> m (Maybe Distance)
