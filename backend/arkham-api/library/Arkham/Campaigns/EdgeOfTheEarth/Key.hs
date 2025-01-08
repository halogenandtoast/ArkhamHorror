module Arkham.Campaigns.EdgeOfTheEarth.Key where

import Arkham.Prelude

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
data EdgeOfTheEarthKey
  = TheInvestigatorsConvincedDyerToAllowTheExpedition
  | TheInvestigatorsDidNotBelieveDyersReport
  | WasKilledInThePlaneCrash
  | SuppliesRecovered
  | Camp_CrashSite
  | Camp_FrozenShores
  | Camp_TreacherousPath
  | Camp_PrecariousIceSheet
  | Camp_BroadSnowdrifts
  | Camp_IcyWastes
  | Camp_RockyCrags
  | Camp_SnowGraves
  | Camp_IcebreakerLanding
  | Camp_FrigidCave
  | Camp_BarrierCamp
  | Camp_RemnantsOfLakesCamp
  | Camp_CrystallineCavern
  | TheTeamBarelyEscapedTheIceShelf
  | TheTeamDefeatedTheHuntingCreatures
  | TheTeamFledToTheMountains
  | LocationsRevealed
  | DrKenslerIsSharingHerResearchWithYou
  | TheInvestigatorsScoutedTheMountainPass
  | EliyahHasConfrontedHisDemons
  | DrSinhaHasConfrontedHerDemons
  | TakadaHasConfrontedHerDemons
  | CookieHasConfrontedHisDemons
  | DyerHasConfrontedHisDemons
  | DanforthHasConfrontedHisDemons
  | ClaypoolHasConfrontedHisDemons
  | EllsworthHasConfrontedHisDemons
  | DrKenslerHasConfrontedHerDemons
  | MemoriesBanished
  | MemoriesDiscovered
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
