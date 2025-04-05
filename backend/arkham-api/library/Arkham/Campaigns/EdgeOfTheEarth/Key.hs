module Arkham.Campaigns.EdgeOfTheEarth.Key where

import Arkham.Prelude

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
data EdgeOfTheEarthKey
  = Camp_BarrierCamp
  | Camp_BroadSnowdrifts
  | Camp_CrashSite
  | Camp_CrystallineCavern
  | Camp_FrigidCave
  | Camp_FrozenShores
  | Camp_IcebreakerLanding
  | Camp_IcyWastes
  | Camp_PrecariousIceSheet
  | Camp_RemnantsOfLakesCamp
  | Camp_RockyCrags
  | Camp_SnowGraves
  | Camp_TreacherousPath
  | ClaypoolHasConfrontedHisDemons
  | CookieHasConfrontedHisDemons
  | DanforthHasConfrontedHisDemons
  | DrKenslerHasAPlan
  | DrKenslerHasConfrontedHerDemons
  | DrKenslerIsOnTheVergeOfUnderstanding
  | DrKenslerIsSharingHerResearchWithYou
  | DrKenslerUnderstandsTheTrueNatureOfTheMiasma
  | DrSinhaHasConfrontedHerDemons
  | DyerHasConfrontedHisDemons
  | EliyahHasConfrontedHisDemons
  | EllsworthHasConfrontedHisDemons
  | LocationsRevealed
  | MemoriesBanished
  | MemoriesDiscovered
  | SealsPlaced
  | SealsRecovered
  | SuppliesRecovered
  | TakadaHasConfrontedHerDemons
  | TheFacilityWasDestroyed
  | TheInvestigatorsConvincedDyerToAllowTheExpedition
  | TheInvestigatorsDidNotBelieveDyersReport
  | TheInvestigatorsScoutedTheCityOutskirts
  | TheInvestigatorsScoutedTheForkedPass
  | TheInvestigatorsScoutedTheMountainPass
  | TheNamelessMadnessEscaped
  | TheNamelessMadnessIsContainedSafelyWithinItsHostForNow
  | TheSealWasUsedImproperly
  | TheSurvivorsOfTheExpeditionWere
  | TheTeamBarelyEscapedTheIceShelf
  | TheTeamClimbedToTheSummit
  | TheTeamDefeatedTheHuntingCreatures
  | TheTeamDiscoveredAnAncientVault
  | TheTeamEscapedTheFacility
  | TheTeamFledToTheMountains
  | TheTeamFoundAnotherWayThroughTheMountains
  | TheTeamFoundTheHiddenTunnel
  | TheTeamPhotographedTheAstronomicalCharts
  | TheTeamReadTheMap
  | TheTeamStudiedTheMuralCarvings
  | TheTeamWasGuidedToTheHiddenTunnel
  | TheTruthOfTheMirageEludesYou
  | WasKilledInThePlaneCrash
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
