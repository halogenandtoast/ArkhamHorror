{- | Campaign-log keys owned by the Circus Ex Mortis campaign.

Like an official campaign (e.g. @TheDunwichLegacyKey@), the campaign owns its own
key enum. It plugs into the core log via the single shared
'Arkham.CampaignLogKey.HomebrewCampaignLogKey' wrapper — no per-campaign wiring
in core. Keys serialize by name through that wrapper.
-}
module Arkham.Homebrew.CircusExMortis.Key (module Arkham.Homebrew.CircusExMortis.Key) where

import Arkham.CampaignLogKey (CampaignLogKey (HomebrewCampaignLogKey), IsCampaignLogKey (..))
import Arkham.Prelude

data CircusExMortisKey
  = -- | One Night Only
    TheRingmasterDoesNotSuspectYou
  | TheRingmasterHasHisEyeOnYou
  | -- | The Primrose Path
    TheInvestigatorsWereLostInTheArkhamWoods
  | TheInvestigatorsBypassedTheIllusions
  | -- | Harm's Way (recorded count: X groups of citizens were saved)
    GroupsOfCitizensWereSavedFromTheCircus
  | -- | All Points West (recorded count: the ringmaster had X days to prepare)
    TheRingmasterHadDaysToPrepare
  | -- | Piper at the Gates of Dawn
    TheInvestigatorsCouldNotEscapeTheCircus
  | TheInvestigatorsClashedWithBlake
  | TheInvestigatorsUnmaskedBlake
  | TheInvestigatorsStruckDownBlake
  | -- | Bacchanalia
    TheInvestigatorsMustFollowTheCult
  | TheInvestigatorsDiscoveredTheRitualsLocation
  | -- | Red Sunrise
    TheInvestigatorsDidNotArriveInTime
  | TheCultRallies
  | -- | Thousand to One
    TheInvestigatorsWereDevouredByTheThousandYoung
  | TheInvestigatorsEndedTheRitual
  | ShubNiggurathReignsOverAnEclipsedWorld
  | ShubNiggurathVanishedWithTheEclipse
  | -- | Recorded by The Prophecy Fulfilled / The Prophecy Unfulfilled (Thousand to One)
    TheProphecyWasFulfilled
  | TheProphecyWasUnfulfilled
  | -- | Recorded set: the Fata Diana destinies ("The one who will strike the
    -- heart", ...). Scenario VIII finds the story card matching each recorded
    -- destiny. Record the destiny story card's card code.
    Destinies
  | -- | Epilogue
    TheNewMoonCircusWasNeverSeenAgain
  | TheNewMoonCircusMaySomedayReturn
  deriving stock (Show, Read, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance IsCampaignLogKey CircusExMortisKey where
  toCampaignLogKey = HomebrewCampaignLogKey . tshow
  fromCampaignLogKey = \case
    HomebrewCampaignLogKey t -> readMay (unpack t)
    _ -> Nothing
