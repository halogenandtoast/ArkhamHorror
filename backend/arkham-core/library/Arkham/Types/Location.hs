{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location
  ( lookupLocation
  , baseLocation
  , isEmptyLocation
  , isRevealed
  , Location(..)
  )
where

import Arkham.Json
import Arkham.Types.AssetId
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Cards
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Coerce
import qualified Data.HashSet as HashSet
import Safe (fromJustNote)

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
  | StudyAberrantGateway' StudyAberrantGateway
  | GuestHall' GuestHall
  | Bedroom' Bedroom
  | Bathroom' Bathroom
  | RitualSite' RitualSite
  | CursedShores' CursedShores
  | GardenDistrict' GardenDistrict
  | Broadmoor' Broadmoor
  | BrackishWaters' BrackishWaters
  | AudubonPark' AudubonPark
  | FauborgMarigny' FauborgMarigny
  | ForgottenMarsh' ForgottenMarsh
  | TrappersCabin' TrappersCabin
  | TwistedUnderbrush' TwistedUnderbrush
  | FoulSwamp' FoulSwamp
  | RitualGrounds' RitualGrounds
  | OvergrownCairns' OvergrownCairns
  | BaseLocation' BaseLocation
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance ActionRunner env => HasActions env Location
deriving anyclass instance LocationRunner env => RunMessage env Location
deriving anyclass instance LocationRunner env => HasModifiersFor env Location

newtype BaseLocation = BaseLocation Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance HasModifiersFor env BaseLocation where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env BaseLocation where
  getActions iid window (BaseLocation attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env BaseLocation where
  runMessage msg (BaseLocation attrs) = BaseLocation <$> runMessage msg attrs

baseLocation
  :: LocationId
  -> Text
  -> Int
  -> GameValue Int
  -> LocationSymbol
  -> [LocationSymbol]
  -> (Attrs -> Attrs)
  -> Location
baseLocation a b c d e f func =
  BaseLocation' . BaseLocation . func $ baseAttrs a b c d e f []

instance HasTraits Location where
  getTraits = locationTraits . locationAttrs

instance HasVictoryPoints Location where
  getVictoryPoints l =
    let Attrs { locationClues, locationVictory } = locationAttrs l
    in if locationClues == 0 then locationVictory else Nothing

instance HasCount ClueCount () Location where
  getCount _ = ClueCount . locationClues . locationAttrs

instance HasCount Shroud () Location where
  getCount _ = Shroud . locationShroud . locationAttrs

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

instance HasModifiers env Location where
  getModifiers _ = pure . concat . toList . locationModifiers . locationAttrs

instance HasId LocationId () Location where
  getId _ = locationId . locationAttrs

instance IsLocation Location where
  isBlocked = isBlocked . locationAttrs

lookupLocation :: LocationId -> Location
lookupLocation lid =
  fromJustNote ("Unknown location: " <> show lid) $ lookup lid allLocations

allLocations :: HashMap LocationId Location
allLocations = mapFromList $ map
  (toFst $ locationId . locationAttrs)
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
  , StudyAberrantGateway' studyAberrantGateway
  , GuestHall' guestHall
  , Bedroom' bedroom
  , Bathroom' bathroom
  , RitualSite' ritualSite
  , CursedShores' cursedShores
  , GardenDistrict' gardenDistrict
  , Broadmoor' broadmoor
  , BrackishWaters' brackishWaters
  , AudubonPark' audubonPark
  , FauborgMarigny' fauborgMarigny
  , ForgottenMarsh' forgottenMarsh
  , TrappersCabin' trappersCabin
  , TwistedUnderbrush' twistedUnderbrush
  , FoulSwamp' foulSwamp
  , RitualGrounds' ritualGrounds
  , OvergrownCairns' overgrownCairns
  ]

isEmptyLocation :: Location -> Bool
isEmptyLocation l = null enemies' && null investigators'
 where
  enemies' = locationEnemies $ locationAttrs l
  investigators' = locationInvestigators $ locationAttrs l

isRevealed :: Location -> Bool
isRevealed = locationRevealed . locationAttrs

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
  StudyAberrantGateway' attrs -> coerce attrs
  GuestHall' attrs -> coerce attrs
  Bedroom' attrs -> coerce attrs
  Bathroom' attrs -> coerce attrs
  RitualSite' attrs -> coerce attrs
  CursedShores' attrs -> coerce attrs
  GardenDistrict' attrs -> coerce attrs
  Broadmoor' attrs -> coerce attrs
  BrackishWaters' attrs -> coerce attrs
  AudubonPark' attrs -> coerce attrs
  FauborgMarigny' attrs -> coerce attrs
  ForgottenMarsh' attrs -> coerce attrs
  TrappersCabin' attrs -> coerce attrs
  TwistedUnderbrush' attrs -> coerce attrs
  FoulSwamp' attrs -> coerce attrs
  RitualGrounds' attrs -> coerce attrs
  OvergrownCairns' attrs -> coerce attrs
  BaseLocation' attrs -> coerce attrs
