{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location
  ( lookupLocation
  , isEmptyLocation
  , isRevealed
  , Location(..)
  )
where

import Arkham.Json
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Cards.ArkhamWoodsCliffside
import Arkham.Types.Location.Cards.ArkhamWoodsOldHouse
import Arkham.Types.Location.Cards.ArkhamWoodsQuietGlade
import Arkham.Types.Location.Cards.ArkhamWoodsTangledThicket
import Arkham.Types.Location.Cards.ArkhamWoodsTwistingPaths
import Arkham.Types.Location.Cards.ArkhamWoodsUnhallowedGround
import Arkham.Types.Location.Cards.Attic
import Arkham.Types.Location.Cards.Cellar
import Arkham.Types.Location.Cards.DowntownArkhamAsylum
import Arkham.Types.Location.Cards.DowntownFirstBankOfArkham
import Arkham.Types.Location.Cards.Easttown
import Arkham.Types.Location.Cards.Graveyard
import Arkham.Types.Location.Cards.Hallway
import Arkham.Types.Location.Cards.MainPath
import Arkham.Types.Location.Cards.MiskatonicUniversity
import Arkham.Types.Location.Cards.Northside
import Arkham.Types.Location.Cards.Parlor
import Arkham.Types.Location.Cards.RitualSite
import Arkham.Types.Location.Cards.Rivertown
import Arkham.Types.Location.Cards.SouthsideHistoricalSociety
import Arkham.Types.Location.Cards.SouthsideMasBoardingHouse
import Arkham.Types.Location.Cards.StMarysHospital
import Arkham.Types.Location.Cards.Study
import Arkham.Types.Location.Cards.YourHouse
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Safe (fromJustNote)

lookupLocation :: LocationId -> Location
lookupLocation lid =
  fromJustNote ("Unkown location: " <> show lid)
    $ HashMap.lookup lid allLocations

instance HasModifiers Location where
  getModifiers = concat . HashMap.elems . locationModifiers . locationAttrs

instance HasId LocationId () Location where
  getId _ = locationId . locationAttrs

instance IsLocation Location where
  isBlocked = isBlocked . locationAttrs

isEmptyLocation :: Location -> Bool
isEmptyLocation l = null enemies' && null investigators'
 where
  enemies' = locationEnemies $ locationAttrs l
  investigators' = locationInvestigators $ locationAttrs l

isRevealed :: Location -> Bool
isRevealed = locationRevealed . locationAttrs

allLocations :: HashMap LocationId Location
allLocations = HashMap.fromList $ map
  (\s -> (locationId . locationAttrs $ s, s))
  [ Study' study
  , Hallway' hallway
  , Attic' attic
  , Cellar' cellar
  , Parlor' parlor
  , YourHouse' yourHouse
  , Rivertown' rivertown
  , SouthsideHistoricalSociety' southsideHistoricalSociety
  , SouthsideMasBoardingHouse' southsideMasBoardingHouse
  , StMarysHospital' stMarysHospital
  , MiskatonicUniversity' miskatonicUniversity
  , DowntownFirstBankOfArkham' downtownFirstBankOfArkham
  , DowntownArkhamAsylum' downtownArkhamAsylum
  , Easttown' easttown
  , Graveyard' graveyard
  , Northside' northside
  , MainPath' mainPath
  , ArkhamWoodsUnhallowedGround' arkhamWoodsUnhallowedGround
  , ArkhamWoodsTwistingPaths' arkhamWoodsTwistingPaths
  , ArkhamWoodsOldHouse' arkhamWoodsOldHouse
  , ArkhamWoodsCliffside' arkhamWoodsCliffside
  , ArkhamWoodsTangledThicket' arkhamWoodsTangledThicket
  , ArkhamWoodsQuietGlade' arkhamWoodsQuietGlade
  , RitualSite' ritualSite
  ]

instance HasTraits Location where
  getTraits = locationTraits . locationAttrs

instance HasVictoryPoints Location where
  getVictoryPoints l =
    let Attrs {..} = locationAttrs l
    in if locationClues == 0 then locationVictory else Nothing

instance HasCount ClueCount () Location where
  getCount _ = ClueCount . locationClues . locationAttrs

instance HasCount DoomCount () Location where
  getCount _ = DoomCount . locationDoom . locationAttrs

instance HasSet EnemyId () Location where
  getSet _ = locationEnemies . locationAttrs

instance HasSet TreacheryId () Location where
  getSet _ = locationTreacheries . locationAttrs

instance HasSet EventId () Location where
  getSet _ = locationEvents . locationAttrs

instance HasSet AssetId () Location where
  getSet _ = locationAssets . locationAttrs

instance HasSet InvestigatorId () Location where
  getSet _ = locationInvestigators . locationAttrs

instance HasSet ConnectedLocationId () Location where
  getSet _ =
    HashSet.map ConnectedLocationId . locationConnectedLocations . locationAttrs

data Location
  = Study' Study
  | Hallway' Hallway
  | Attic' Attic
  | Cellar' Cellar
  | Parlor' Parlor
  | YourHouse' YourHouse
  | Rivertown' Rivertown
  | SouthsideHistoricalSociety' SouthsideHistoricalSociety
  | SouthsideMasBoardingHouse' SouthsideMasBoardingHouse
  | StMarysHospital' StMarysHospital
  | MiskatonicUniversity' MiskatonicUniversity
  | DowntownFirstBankOfArkham' DowntownFirstBankOfArkham
  | DowntownArkhamAsylum' DowntownArkhamAsylum
  | Easttown' Easttown
  | Graveyard' Graveyard
  | Northside' Northside
  | MainPath' MainPath
  | ArkhamWoodsUnhallowedGround' ArkhamWoodsUnhallowedGround
  | ArkhamWoodsTwistingPaths' ArkhamWoodsTwistingPaths
  | ArkhamWoodsOldHouse' ArkhamWoodsOldHouse
  | ArkhamWoodsCliffside' ArkhamWoodsCliffside
  | ArkhamWoodsTangledThicket' ArkhamWoodsTangledThicket
  | ArkhamWoodsQuietGlade' ArkhamWoodsQuietGlade
  | RitualSite' RitualSite
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance (ActionRunner env investigator) => HasActions env investigator Location
deriving anyclass instance (LocationRunner env) => RunMessage env Location

locationAttrs :: Location -> Attrs
locationAttrs = \case
  Study' attrs -> coerce attrs
  Hallway' attrs -> coerce attrs
  Attic' attrs -> coerce attrs
  Cellar' attrs -> coerce attrs
  Parlor' attrs -> coerce attrs
  YourHouse' attrs -> coerce attrs
  Rivertown' attrs -> coerce attrs
  SouthsideHistoricalSociety' attrs -> coerce attrs
  SouthsideMasBoardingHouse' attrs -> coerce attrs
  StMarysHospital' attrs -> coerce attrs
  MiskatonicUniversity' attrs -> coerce attrs
  DowntownFirstBankOfArkham' attrs -> coerce attrs
  DowntownArkhamAsylum' attrs -> coerce attrs
  Easttown' attrs -> coerce attrs
  Graveyard' attrs -> coerce attrs
  Northside' attrs -> coerce attrs
  MainPath' attrs -> coerce attrs
  ArkhamWoodsUnhallowedGround' attrs -> coerce attrs
  ArkhamWoodsTwistingPaths' attrs -> coerce attrs
  ArkhamWoodsOldHouse' attrs -> coerce attrs
  ArkhamWoodsCliffside' attrs -> coerce attrs
  ArkhamWoodsTangledThicket' attrs -> coerce attrs
  ArkhamWoodsQuietGlade' attrs -> coerce attrs
  RitualSite' attrs -> coerce attrs
