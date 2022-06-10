module Arkham.Classes.HasDistance where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Distance
import Arkham.LocationId

class HasDistance a where
  getDistance' :: a -> LocationId -> LocationId -> GameT (Maybe Distance)
