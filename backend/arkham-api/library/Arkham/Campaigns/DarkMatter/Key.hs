module Arkham.Campaigns.DarkMatter.Key where

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
  | -- | Tallies: per-investigator count (see 'Arkham.Campaigns.DarkMatter.Helpers')
    Memories
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
