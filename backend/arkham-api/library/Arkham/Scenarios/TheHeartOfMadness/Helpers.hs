module Arkham.Scenarios.TheHeartOfMadness.Helpers where

import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Classes.HasGame
import Arkham.I18n
import Arkham.Id
import Arkham.Label
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted.Queue
import Arkham.Prelude
import Arkham.Target

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "toTheForbiddenPeaks" a

sealAtLocationOf :: Applicative m => InvestigatorId -> m Bool
sealAtLocationOf _iid = pure False

placeSeal :: (ReverseQueue m, Targetable target) => target -> Seal -> m ()
placeSeal target = push . PlaceSeal (toTarget target)

activateSeal :: ReverseQueue m => SealKind -> m ()
activateSeal = push . ActivateSeal

getLocationsOnSameSpoke :: HasGame m => Text -> LocationMatcher -> m [LocationId]
getLocationsOnSameSpoke facility matcher = case find (elem facility) spokes of
  Nothing -> pure []
  Just spoke -> select $ matcher <> mapOneOf (LocationWithLabel . mkLabel) spoke
 where
  spoke1 = ["facility1", "facility4", "facility7"]
  spoke2 = ["facility2", "facility5", "facility8"]
  spoke3 = ["facility3", "facility6", "facility9"]
  spoke4 = ["facility10", "facility12", "facility14"]
  spoke5 = ["facility11", "facility13", "facility15"]
  spokes = [spoke1, spoke2, spoke3, spoke4, spoke5]

getLocationsOnSameRing :: HasGame m => Text -> LocationMatcher -> m [LocationId]
getLocationsOnSameRing facility matcher = case find (elem facility) rings of
  Nothing -> pure []
  Just ring -> select $ matcher <> mapOneOf (LocationWithLabel . mkLabel) ring
 where
  ring1 = ["facility1", "facility2", "facility3", "facility14", "facility15"]
  ring2 = ["facility4", "facility5", "facility6", "facility12", "facility13"]
  ring3 = ["facility7", "facility8", "facility9", "facility10", "facility11"]
  rings = [ring1, ring2, ring3]
