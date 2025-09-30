module Arkham.Scenarios.InTheClutchesOfChaos.Helpers where

import Arkham.Ability
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.I18n
import Arkham.Id
import Arkham.Label ()
import Arkham.Location.BreachStatus
import Arkham.Location.Types (Field (..), LocationAttrs (locationBreaches))
import Arkham.Matcher
import Arkham.Message (Message (PlaceBreaches, RemoveBreaches))
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Queue
import Arkham.Prelude
import Arkham.Projection
import Arkham.Target

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "unionAndDisillusion" a

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
  select $ LocationMatchAny $ map LocationWithLabel lbls

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

removeBreaches :: (Targetable target, ReverseQueue m) => target -> Int -> m ()
removeBreaches target = push . RemoveBreaches (toTarget target)

placeBreaches :: (Targetable target, ReverseQueue m) => target -> Int -> m ()
placeBreaches target = push . PlaceBreaches (toTarget target)

resolveIncursion :: (ReverseQueue m, ToId location LocationId) => location -> m ()
resolveIncursion = push . Msg.Incursion . asId
