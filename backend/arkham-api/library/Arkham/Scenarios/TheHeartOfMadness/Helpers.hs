module Arkham.Scenarios.TheHeartOfMadness.Helpers where

import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.ChaosToken
import Arkham.Classes.Entity
import Arkham.Classes.HasChaosTokenValue
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Helpers.Scenario (toChaosTokenValue)
import Arkham.I18n
import Arkham.Id
import Arkham.Label
import Arkham.Layout
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted.Queue
import Arkham.Prelude
import Arkham.Scenario.Types
import Arkham.Target
import Arkham.Trait (Trait (Ancient))

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

theHeartOfMadnessLayout :: [GridTemplateRow]
theHeartOfMadnessLayout =
  [ ".          .          .          facility1      .          .          ."
  , "facility2  .          .          facility1      .          .          facility3"
  , "facility2  .          .          facility4      .          .          facility3"
  , ".          facility5  .          facility4      .          facility6  ."
  , ".          facility5  .          facility7      .          facility6  ."
  , ".          .          facility8  facility7      facility9  .          ."
  , ".          .          facility8  theGateOfYquaa facility9  .          ."
  , ".          .          .          theGateOfYquaa .          .          ."
  , ".          .          facility10 .              facility11 .          ."
  , ".          .          facility10 .              facility11 .          ."
  , ".          facility12 .          .              .          facility13 ."
  , ".          facility12 .          .              .          facility13 ."
  , "facility14 .          .          .              .          .          facility15"
  , "facility14 .          .          .              .          .          facility15"
  ]

getChaosTokenValueFromScenario
  :: (HasCallStack, HasGame m, Entity s, EntityAttrs s ~ ScenarioAttrs)
  => InvestigatorId -> ChaosTokenFace -> s -> m ChaosTokenValue
getChaosTokenValueFromScenario iid tokenFace (toAttrs -> attrs) = case tokenFace of
  Skull -> do
    ancient <- selectAny $ withTrait Ancient <> EnemyAt (locationWithInvestigator iid)
    pure
      $ if ancient
        then toChaosTokenValue attrs Skull 1 3
        else toChaosTokenValue attrs Skull 2 4
  Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 1)
  Tablet -> pure $ ChaosTokenValue Tablet (NegativeModifier 3)
  ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
  otherFace -> getChaosTokenValue iid otherFace attrs

connectAllLocations :: ReverseQueue m => m ()
connectAllLocations = do
  connectLocations "facility1" "facility2"
  connectLocations "facility1" "facility3"
  connectLocations "facility1" "facility4"
  connectLocations "facility2" "facility5"
  connectLocations "facility2" "facility14"
  connectLocations "facility3" "facility6"
  connectLocations "facility3" "facility15"
  connectLocations "facility4" "facility5"
  connectLocations "facility4" "facility6"
  connectLocations "facility4" "facility7"
  connectLocations "facility5" "facility8"
  connectLocations "facility5" "facility12"
  connectLocations "facility6" "facility9"
  connectLocations "facility6" "facility13"
  connectLocations "facility7" "facility8"
  connectLocations "facility7" "facility9"
  connectLocations "facility7" "theGateOfYquaa"
  connectLocations "facility8" "facility10"
  connectLocations "facility8" "theGateOfYquaa"
  connectLocations "facility9" "facility11"
  connectLocations "facility9" "theGateOfYquaa"
  connectLocations "facility10" "facility11"
  connectLocations "facility10" "facility12"
  connectLocations "facility10" "theGateOfYquaa"
  connectLocations "facility11" "facility13"
  connectLocations "facility11" "theGateOfYquaa"
  connectLocations "facility12" "facility13"
  connectLocations "facility12" "facility14"
  connectLocations "facility13" "facility15"
  connectLocations "facility14" "facility15"
 where
  connectLocations a b = do
    l1 <- selectJust $ LocationWithLabel a
    l2 <- selectJust $ LocationWithLabel b
    push $ AddDirectConnection l1 l2
    push $ AddDirectConnection l2 l1
