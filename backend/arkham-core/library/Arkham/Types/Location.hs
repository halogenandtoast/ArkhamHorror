{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location
  ( lookupLocation
  , isEmptyLocation
  , Location(..)
  )
where

import Arkham.Json
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.EnemyId
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

instance HasId LocationId () Location where
  getId _ = locationId . locationAttrs

instance IsLocation Location where
  isBlocked = isBlocked . locationAttrs

isEmptyLocation :: Location -> Bool
isEmptyLocation l = null enemies' && null investigators'
 where
  enemies' = locationEnemies $ locationAttrs l
  investigators' = locationInvestigators $ locationAttrs l

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

instance (ActionRunner env investigator) => HasActions env investigator Location where
  getActions i window = \case
    Study' l -> getActions i window l
    Hallway' l -> getActions i window l
    Attic' l -> getActions i window l
    Cellar' l -> getActions i window l
    Parlor' l -> getActions i window l
    YourHouse' l -> getActions i window l
    Rivertown' l -> getActions i window l
    SouthsideHistoricalSociety' l -> getActions i window l
    SouthsideMasBoardingHouse' l -> getActions i window l
    StMarysHospital' l -> getActions i window l
    MiskatonicUniversity' l -> getActions i window l
    DowntownFirstBankOfArkham' l -> getActions i window l
    DowntownArkhamAsylum' l -> getActions i window l
    Easttown' l -> getActions i window l
    Graveyard' l -> getActions i window l
    Northside' l -> getActions i window l
    MainPath' l -> getActions i window l
    ArkhamWoodsUnhallowedGround' l -> getActions i window l
    ArkhamWoodsTwistingPaths' l -> getActions i window l
    ArkhamWoodsOldHouse' l -> getActions i window l
    ArkhamWoodsCliffside' l -> getActions i window l
    ArkhamWoodsTangledThicket' l -> getActions i window l
    ArkhamWoodsQuietGlade' l -> getActions i window l
    RitualSite' l -> getActions i window l

instance (LocationRunner env) => RunMessage env Location where
  runMessage msg = \case
    Study' x -> Study' <$> runMessage msg x
    Hallway' x -> Hallway' <$> runMessage msg x
    Attic' x -> Attic' <$> runMessage msg x
    Cellar' x -> Cellar' <$> runMessage msg x
    Parlor' x -> Parlor' <$> runMessage msg x
    YourHouse' x -> YourHouse' <$> runMessage msg x
    Rivertown' x -> Rivertown' <$> runMessage msg x
    SouthsideHistoricalSociety' x ->
      SouthsideHistoricalSociety' <$> runMessage msg x
    SouthsideMasBoardingHouse' x ->
      SouthsideMasBoardingHouse' <$> runMessage msg x
    StMarysHospital' x -> StMarysHospital' <$> runMessage msg x
    MiskatonicUniversity' x -> MiskatonicUniversity' <$> runMessage msg x
    DowntownFirstBankOfArkham' x ->
      DowntownFirstBankOfArkham' <$> runMessage msg x
    DowntownArkhamAsylum' x -> DowntownArkhamAsylum' <$> runMessage msg x
    Easttown' x -> Easttown' <$> runMessage msg x
    Graveyard' x -> Graveyard' <$> runMessage msg x
    Northside' x -> Northside' <$> runMessage msg x
    MainPath' x -> MainPath' <$> runMessage msg x
    ArkhamWoodsUnhallowedGround' x ->
      ArkhamWoodsUnhallowedGround' <$> runMessage msg x
    ArkhamWoodsTwistingPaths' x ->
      ArkhamWoodsTwistingPaths' <$> runMessage msg x
    ArkhamWoodsOldHouse' x -> ArkhamWoodsOldHouse' <$> runMessage msg x
    ArkhamWoodsCliffside' x -> ArkhamWoodsCliffside' <$> runMessage msg x
    ArkhamWoodsTangledThicket' x ->
      ArkhamWoodsTangledThicket' <$> runMessage msg x
    ArkhamWoodsQuietGlade' x -> ArkhamWoodsQuietGlade' <$> runMessage msg x
    RitualSite' x -> RitualSite' <$> runMessage msg x
