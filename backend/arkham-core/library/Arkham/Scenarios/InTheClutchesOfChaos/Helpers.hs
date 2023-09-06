module Arkham.Scenarios.InTheClutchesOfChaos.Helpers
where

import Arkham.Prelude

import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Label ()
import Arkham.Location.BreachStatus
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

sampleLocations :: (HasGame m, MonadRandom m) => Int -> m [LocationId]
sampleLocations n = do
  lbls <-
    sampleN n $
      "merchantDistrict"
        :| [ "rivertown"
           , "hangmansHill"
           , "uptown"
           , "southside"
           , "frenchHill"
           , "silverTwilightLodge"
           , "southChurch"
           ]
  selectList $ LocationMatchAny $ map LocationWithLabel lbls

getBreaches :: HasGame m => LocationId -> m Int
getBreaches = fieldMap LocationBreaches (maybe 0 countBreaches)
