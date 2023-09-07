module Arkham.Scenarios.InTheClutchesOfChaos.Helpers
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Label ()
import Arkham.Location.BreachStatus
import Arkham.Location.Types (Field (..), LocationAttrs (locationBreaches))
import Arkham.Matcher
import Arkham.Projection

sampleLocations :: (HasGame m, MonadRandom m) => Int -> m [LocationId]
sampleLocations n = do
  lbls <-
    sampleN n
      $ "merchantDistrict"
        :| [ "rivertown"
           , "hangmansHill"
           , "uptown"
           , "southside"
           , "frenchHill"
           , "silverTwilightLodge"
           , "southChurch"
           ]
  selectList $ LocationMatchAny $ map LocationWithLabel lbls

sampleLocation :: (HasGame m, MonadRandom m) => m LocationId
sampleLocation = do
  result <- sampleLocations 1
  case result of
    [] -> error "No locations found"
    (x : _) -> pure x

getBreaches :: HasGame m => LocationId -> m Int
getBreaches = fieldMap LocationBreaches (maybe 0 countBreaches)

withBreaches :: LocationAttrs -> Criterion -> Criterion
withBreaches attrs =
  let breaches = maybe 0 countBreaches $ locationBreaches attrs
   in if breaches > 0 then id else const Never

countLocationBreaches :: LocationAttrs -> Int
countLocationBreaches = maybe 0 countBreaches . locationBreaches
