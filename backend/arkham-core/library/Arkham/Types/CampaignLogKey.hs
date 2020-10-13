module Arkham.Types.CampaignLogKey where

import Arkham.Json
import ClassyPrelude

data CampaignLogKey
  = GhoulPriestIsStillAlive
  | YourHouseIsStillStanding
  | YourHouseHasBurnedToTheGround
  | LitaWasForcedToFindOthersToHelpHerCause
  | CultistsWeInterrogated
  | CultistsWhoGotAway
  | ItIsPastMidnight
  | ArkhamSuccumbedToUmordhothsTerribleVengeance
  | TheRitualToSummonUmordhothWasBroken
  | TheInvestigatorsRepelledUmordoth
  | TheInvestigatorsSacrificedLitaChantlerToUmordhoth
  | TheRougarouContinuesToHauntTheBayou
  | TheRougarouIsDestroyed
  | TheRougarouEscapedAndYouEmbracedTheCurse
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, Hashable, FromJSONKey)

