module Arkham.Classes.HasDistance where

import Arkham.Prelude

import Arkham.Classes.HasGame
import Arkham.Distance
import Arkham.Id

class HasDistance a where
  getDistance' :: HasGame m => a -> LocationId -> LocationId -> m (Maybe Distance)
