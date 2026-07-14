{- | Campaign-log keys owned by the Dark Matter campaign.

Like an official campaign (e.g. @TheDunwichLegacyKey@), the campaign owns its own
key enum. It plugs into the core log via the single shared
'Arkham.CampaignLogKey.HomebrewCampaignLogKey' wrapper — no per-campaign wiring
in core. Keys serialize by name through that wrapper.
-}
module Arkham.Homebrew.DarkMatter.Key (module Arkham.Homebrew.DarkMatter.Key) where

import Arkham.CampaignLogKey (CampaignLogKey (HomebrewCampaignLogKey), IsCampaignLogKey (..))
import Arkham.Prelude

data DarkMatterKey
  = -- | Scenario I: The Tatterdemalion
    YouWereTransportedToTheVirtualDreamlandsByMaja
  | YouEnteredTheVirtualDreamlandsByYourOwnMeans
  | -- | recorded per investigator
    HasBeenInfectedByTheCybervirus
  | -- | Scenario II: Electric Nightmare
    YouAreTrappedInAVirtualNightmare
  | YouPartiallyRestoredTheSanityOfK2PS187
  | YouFullyRestoredTheSanityOfK2PS187
  | -- | Interlude I: Mission Briefing
    YouHaveWatchedThePerformanceOfHeirToCarcosa
  | -- | Scenario IIIa: Lost Quantum
    TheElbrusStationHasBeenLostInTheQuantumRealm
  | TheElbrusStationHasBeenFullyStabilized
  | YouHaveWitnessedThePrimordialChaos
  | -- | Scenario IIIb: In the Shadow of Earth
    YouHaveRescuedTheRemainingCrewOfTheNostalgiaII
  | TheNostalgiaIIHasBeenSaved
  | YouCouldntSaveTheCrewOfTheNostalgiaII
  | AllInvestigatorsHaveBeenCorruptedByTheEarth
  | -- | Scenario IIIc: Strange Moons
    YouAreNotInGoodStandingWithTheMiGo
  | YouAreAlliedWithTheMiGo
  | YouHaveUncoveredTheCultistsInhumanMethods
  | -- | Scenario V: Fragment of Carcosa
    TheInvestigatorsAreTrappedWithinCarcosa
  | TheInvestigatorsKnowOfTheAbjurationOfTheThrone
  | -- | Scenario VI: Starfall
    TheSunWentSupernovaAndWipedOutMostOfTheSolarSystem
  | MiGoSafelyReturnedToTheirHomeWorld
  | HopeWasShieldedFromTheBlast
  | TheUCCEscapedToAnotherGalaxy
  | -- | recorded per investigator
    LivedTheRemainderOfTheirDaysWithHope
  | -- | recorded per investigator
    LivedTheRemainderOfTheirDaysOnParadise
  | YouHaveReturnedToThePalacesOfDimCarcosa
  | -- | recorded per investigator
    WasCaughtInTheSupernovasBlast
  | TheInvestigatorsEscapedHastursGrasp
  | -- | Epilogue
    TheRealmOfCarcosaOvertookOurUniverse
  | HasturAndTassildaAreImprisonedInCarcosaOnceMore
  | -- | Tallies: campaign-wide count
    ImpendingDoom
  | -- | Tallies: per-investigator count (see 'Arkham.Homebrew.DarkMatter.Helpers')
    Memories
  deriving stock (Show, Read, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance IsCampaignLogKey DarkMatterKey where
  toCampaignLogKey = HomebrewCampaignLogKey . tshow
  fromCampaignLogKey = \case
    HomebrewCampaignLogKey t -> readMay (unpack t)
    _ -> Nothing
