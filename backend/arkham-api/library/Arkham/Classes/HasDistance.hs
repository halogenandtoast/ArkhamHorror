module Arkham.Classes.HasDistance where

import Arkham.Prelude

import Arkham.Classes.HasGame
import Arkham.Distance
import Arkham.Id
import Arkham.Tracing

class HasDistance a where
  getDistance' :: (HasGame m, Tracing m) => a -> LocationId -> LocationId -> m (Maybe Distance)
