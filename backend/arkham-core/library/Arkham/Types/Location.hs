module Arkham.Types.Location
  ( module Arkham.Types.Location
  )
where

import Arkham.Prelude

import Arkham.Types.Modifier
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.AssetId
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Classes
import Arkham.Types.Card
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Cards
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.TreacheryId

createLocation :: IsCard a => a -> Location
createLocation a = lookupLocation (LocationId $ getCardCode a)

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
  | MiskatonicQuad' MiskatonicQuad
  | HumanitiesBuilding' HumanitiesBuilding
  | OrneLibrary' OrneLibrary
  | StudentUnion' StudentUnion
  | Dormitories' Dormitories
  | AdministrationBuilding' AdministrationBuilding
  | FacultyOfficesTheNightIsStillYoung' FacultyOfficesTheNightIsStillYoung
  | FacultyOfficesTheHourIsLate' FacultyOfficesTheHourIsLate
  | ScienceBuilding' ScienceBuilding
  | AlchemyLabs' AlchemyLabs
  | LaBellaLuna' LaBellaLuna
  | CloverClubLounge' CloverClubLounge
  | CloverClubBar' CloverClubBar
  | CloverClubCardroom' CloverClubCardroom
  | DarkenedHall' DarkenedHall
  | ArtGallery' ArtGallery
  | VipArea' VipArea
  | BackAlley' BackAlley
  | MuseumEntrance' MuseumEntrance
  | MuseumHalls' MuseumHalls
  | SecurityOffice_128' SecurityOffice_128
  | SecurityOffice_129' SecurityOffice_129
  | AdministrationOffice_130' AdministrationOffice_130
  | AdministrationOffice_131' AdministrationOffice_131
  | ExhibitHallAthabaskanExhibit' ExhibitHallAthabaskanExhibit
  | ExhibitHallMedusaExhibit' ExhibitHallMedusaExhibit
  | ExhibitHallNatureExhibit' ExhibitHallNatureExhibit
  | ExhibitHallEgyptianExhibit' ExhibitHallEgyptianExhibit
  | ExhibitHallHallOfTheDead' ExhibitHallHallOfTheDead
  | ExhibitHallRestrictedHall' ExhibitHallRestrictedHall
  | PassengerCar_167' PassengerCar_167
  | PassengerCar_168' PassengerCar_168
  | PassengerCar_169' PassengerCar_169
  | PassengerCar_170' PassengerCar_170
  | PassengerCar_171' PassengerCar_171
  | SleepingCar' SleepingCar
  | DiningCar' DiningCar
  | ParlorCar' ParlorCar
  | EngineCar_175' EngineCar_175
  | EngineCar_176' EngineCar_176
  | EngineCar_177' EngineCar_177
  | VillageCommons' VillageCommons
  | BishopsBrook_202' BishopsBrook_202
  | BishopsBrook_203' BishopsBrook_203
  | BurnedRuins_204' BurnedRuins_204
  | BurnedRuins_205' BurnedRuins_205
  | OsbornsGeneralStore_206' OsbornsGeneralStore_206
  | OsbornsGeneralStore_207' OsbornsGeneralStore_207
  | CongregationalChurch_208' CongregationalChurch_208
  | CongregationalChurch_209' CongregationalChurch_209
  | HouseInTheReeds_210' HouseInTheReeds_210
  | HouseInTheReeds_211' HouseInTheReeds_211
  | Schoolhouse_212' Schoolhouse_212
  | Schoolhouse_213' Schoolhouse_213
  | TheHiddenChamber' TheHiddenChamber
  | DunwichVillage_242' DunwichVillage_242
  | DunwichVillage_243' DunwichVillage_243
  | ColdSpringGlen_244' ColdSpringGlen_244
  | ColdSpringGlen_245' ColdSpringGlen_245
  | TenAcreMeadow_246' TenAcreMeadow_246
  | TenAcreMeadow_247' TenAcreMeadow_247
  | BlastedHeath_248' BlastedHeath_248
  | BlastedHeath_249' BlastedHeath_249
  | WhateleyRuins_250' WhateleyRuins_250
  | WhateleyRuins_251' WhateleyRuins_251
  | DevilsHopYard_252' DevilsHopYard_252
  | DevilsHopYard_253' DevilsHopYard_253
  | StudyAberrantGateway' StudyAberrantGateway
  | GuestHall' GuestHall
  | Bedroom' Bedroom
  | Bathroom' Bathroom
  | HoleInTheWall' HoleInTheWall
  | ReturnToAttic' ReturnToAttic
  | FarAboveYourHouse' FarAboveYourHouse
  | ReturnToCellar' ReturnToCellar
  | DeepBelowYourHouse' DeepBelowYourHouse
  | EasttownArkhamPoliceStation' EasttownArkhamPoliceStation
  | NorthsideTrainStation' NorthsideTrainStation
  | MiskatonicUniversityMiskatonicMuseum' MiskatonicUniversityMiskatonicMuseum
  | RivertownAbandonedWarehouse' RivertownAbandonedWarehouse
  | ArkhamWoodsGreatWillow' ArkhamWoodsGreatWillow
  | ArkhamWoodsLakeside' ArkhamWoodsLakeside
  | ArkhamWoodsCorpseRiddenClearing' ArkhamWoodsCorpseRiddenClearing
  | ArkhamWoodsWoodenBridge' ArkhamWoodsWoodenBridge
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
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance ActionRunner env => HasActions env Location
deriving anyclass instance
  ( HasPhase env
  , HasCount CardCount env InvestigatorId
  , HasCount ClueCount env LocationId
  , HasCount ResourceCount env InvestigatorId
  , HasId (Maybe StoryEnemyId) env CardCode
  , HasId (Maybe StoryAssetId) env CardCode
  )
  => HasModifiersFor env Location

instance LocationRunner env => RunMessage env Location where
  runMessage msg l = do
    modifiers' <- getModifiersFor (toSource l) (toTarget l) ()
    let msg' = if any isBlank modifiers' then Blanked msg else msg
    defaultRunMessage msg' l

instance Entity Location where
  type EntityId Location = LocationId
  type EntityAttrs Location = LocationAttrs

instance NamedEntity Location where
  toName = toName . toAttrs

instance TargetEntity Location where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Location where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

newtype BaseLocation = BaseLocation LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor env BaseLocation where
  getModifiersFor = noModifiersFor

instance HasName env Location where
  getName = getName . toAttrs

instance ActionRunner env => HasActions env BaseLocation where
  getActions iid window (BaseLocation attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env BaseLocation where
  runMessage msg (BaseLocation attrs) = BaseLocation <$> runMessage msg attrs

baseLocation
  :: LocationId
  -> Name
  -> Int
  -> GameValue Int
  -> LocationSymbol
  -> [LocationSymbol]
  -> (LocationAttrs -> LocationAttrs)
  -> Location
baseLocation a b c d e f func = BaseLocation' . BaseLocation . func $ baseAttrs
  a
  b
  EncounterSet.TheGathering
  c
  d
  e
  f
  []

instance IsCard Location where
  getCardId = getCardId . toAttrs
  getCardCode = getCardCode . toAttrs
  getTraits = getTraits . toAttrs
  getKeywords = getKeywords . toAttrs

instance HasVictoryPoints Location where
  getVictoryPoints l =
    let LocationAttrs { locationClues, locationVictory } = toAttrs l
    in if locationClues == 0 then locationVictory else Nothing

instance HasCount ClueCount env Location where
  getCount = pure . ClueCount . locationClues . toAttrs

instance HasCount Shroud env Location where
  getCount = pure . Shroud . locationShroud . toAttrs

instance HasCount DoomCount env Location where
  getCount = pure . DoomCount . locationDoom . toAttrs

instance HasList UnderneathCard env Location where
  getList = getList . toAttrs

instance HasSet EnemyId env Location where
  getSet = pure . locationEnemies . toAttrs

instance HasSet TreacheryId env Location where
  getSet = pure . locationTreacheries . toAttrs

instance HasSet EventId env Location where
  getSet = pure . locationEvents . toAttrs

instance HasSet AssetId env Location where
  getSet = pure . locationAssets . toAttrs

instance HasSet InvestigatorId env Location where
  getSet = pure . locationInvestigators . toAttrs

instance HasSet ConnectedLocationId env Location where
  getSet =
    pure . mapSet ConnectedLocationId . locationConnectedLocations . toAttrs

instance HasId LocationId env Location where
  getId = pure . toId

instance HasId (Maybe LocationId) env (Direction, Location) where
  getId (dir, location) = getId (dir, toAttrs location)

getLocationName :: Location -> LocationName
getLocationName = locationName . toAttrs

lookupLocation :: LocationId -> Location
lookupLocation lid =
  fromJustNote ("Unknown location: " <> show lid) $ lookup lid allLocations

allLocations :: HashMap LocationId Location
allLocations = mapFrom
  toId
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
  , MiskatonicQuad' miskatonicQuad
  , HumanitiesBuilding' humanitiesBuilding
  , OrneLibrary' orneLibrary
  , StudentUnion' studentUnion
  , Dormitories' dormitories
  , AdministrationBuilding' administrationBuilding
  , FacultyOfficesTheNightIsStillYoung' facultyOfficesTheNightIsStillYoung
  , FacultyOfficesTheHourIsLate' facultyOfficesTheHourIsLate
  , ScienceBuilding' scienceBuilding
  , AlchemyLabs' alchemyLabs
  , LaBellaLuna' laBellaLuna
  , CloverClubLounge' cloverClubLounge
  , CloverClubBar' cloverClubBar
  , CloverClubCardroom' cloverClubCardroom
  , DarkenedHall' darkenedHall
  , ArtGallery' artGallery
  , VipArea' vipArea
  , BackAlley' backAlley
  , MuseumEntrance' museumEntrance
  , MuseumHalls' museumHalls
  , SecurityOffice_128' securityOffice_128
  , SecurityOffice_129' securityOffice_129
  , AdministrationOffice_130' administrationOffice_130
  , AdministrationOffice_131' administrationOffice_131
  , ExhibitHallAthabaskanExhibit' exhibitHallAthabaskanExhibit
  , ExhibitHallMedusaExhibit' exhibitHallMedusaExhibit
  , ExhibitHallNatureExhibit' exhibitHallNatureExhibit
  , ExhibitHallEgyptianExhibit' exhibitHallEgyptianExhibit
  , ExhibitHallHallOfTheDead' exhibitHallHallOfTheDead
  , ExhibitHallRestrictedHall' exhibitHallRestrictedHall
  , PassengerCar_167' passengerCar_167
  , PassengerCar_168' passengerCar_168
  , PassengerCar_169' passengerCar_169
  , PassengerCar_170' passengerCar_170
  , PassengerCar_171' passengerCar_171
  , SleepingCar' sleepingCar
  , DiningCar' diningCar
  , ParlorCar' parlorCar
  , EngineCar_175' engineCar_175
  , EngineCar_176' engineCar_176
  , EngineCar_177' engineCar_177
  , VillageCommons' villageCommons
  , BishopsBrook_202' bishopsBrook_202
  , BishopsBrook_203' bishopsBrook_203
  , BurnedRuins_204' burnedRuins_204
  , BurnedRuins_205' burnedRuins_205
  , OsbornsGeneralStore_206' osbornsGeneralStore_206
  , OsbornsGeneralStore_207' osbornsGeneralStore_207
  , CongregationalChurch_208' congregationalChurch_208
  , CongregationalChurch_209' congregationalChurch_209
  , HouseInTheReeds_210' houseInTheReeds_210
  , HouseInTheReeds_211' houseInTheReeds_211
  , Schoolhouse_212' schoolhouse_212
  , Schoolhouse_213' schoolhouse_213
  , TheHiddenChamber' theHiddenChamber
  , ColdSpringGlen_244' coldSpringGlen_244
  , ColdSpringGlen_245' coldSpringGlen_245
  , TenAcreMeadow_246' tenAcreMeadow_246
  , TenAcreMeadow_247' tenAcreMeadow_247
  , BlastedHeath_248' blastedHeath_248
  , BlastedHeath_249' blastedHeath_249
  , WhateleyRuins_250' whateleyRuins_250
  , WhateleyRuins_251' whateleyRuins_251
  , DevilsHopYard_252' devilsHopYard_252
  , DevilsHopYard_253' devilsHopYard_253
  , StudyAberrantGateway' studyAberrantGateway
  , GuestHall' guestHall
  , Bedroom' bedroom
  , Bathroom' bathroom
  , HoleInTheWall' holeInTheWall
  , ReturnToAttic' returnToAttic
  , FarAboveYourHouse' farAboveYourHouse
  , ReturnToCellar' returnToCellar
  , DeepBelowYourHouse' deepBelowYourHouse
  , EasttownArkhamPoliceStation' easttownArkhamPoliceStation
  , NorthsideTrainStation' northsideTrainStation
  , MiskatonicUniversityMiskatonicMuseum' miskatonicUniversityMiskatonicMuseum
  , RivertownAbandonedWarehouse' rivertownAbandonedWarehouse
  , ArkhamWoodsGreatWillow' arkhamWoodsGreatWillow
  , ArkhamWoodsLakeside' arkhamWoodsLakeside
  , ArkhamWoodsCorpseRiddenClearing' arkhamWoodsCorpseRiddenClearing
  , ArkhamWoodsWoodenBridge' arkhamWoodsWoodenBridge
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
  enemies' = locationEnemies $ toAttrs l
  investigators' = locationInvestigators $ toAttrs l

isRevealed :: Location -> Bool
isRevealed = locationRevealed . toAttrs
