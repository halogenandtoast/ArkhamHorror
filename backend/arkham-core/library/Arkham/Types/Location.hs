module Arkham.Types.Location
  ( module Arkham.Types.Location
  )
where

import Arkham.Prelude

import Arkham.EncounterCard
import Control.Lens.Wrapped
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Direction
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Cards
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.TreacheryId
import Data.UUID (nil)

createLocation :: IsCard a => a -> Location
createLocation a = lookupLocation (a ^. defL . cardCodeL) (LocationId $ a ^. cardIdL)

toLocationSymbol :: Location -> LocationSymbol
toLocationSymbol = locationSymbol . toAttrs

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
  | BaseOfTheHill' BaseOfTheHill
  | AscendingPath' AscendingPath
  | SentinelPeak' SentinelPeak
  | SlaughteredWoods' SlaughteredWoods
  | EerieGlade' EerieGlade
  | DestroyedPath' DestroyedPath
  | FrozenSpring' FrozenSpring
  | DimensionalGap' DimensionalGap
  | ATearInThePath' ATearInThePath
  | UprootedWoods' UprootedWoods
  | LostMemories' LostMemories
  | AnotherDimension' AnotherDimension
  | TheEdgeOfTheUniverse' TheEdgeOfTheUniverse
  | TearThroughTime' TearThroughTime
  | TearThroughSpace' TearThroughSpace
  | PrismaticCascade' PrismaticCascade
  | EndlessBridge' EndlessBridge
  | StepsOfYhagharl' StepsOfYhagharl
  | DimensionalDoorway' DimensionalDoorway
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

instance (HasSet UnengagedEnemyId env LocationId, LocationRunner env) => RunMessage env Location where
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

instance HasCardDef Location where
  defL = genericDefL

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
  -> CardCode
  -> Int
  -> GameValue Int
  -> LocationSymbol
  -> [LocationSymbol]
  -> (LocationAttrs -> LocationAttrs)
  -> Location
baseLocation a b c d e f func =
  BaseLocation' . BaseLocation . func $ baseAttrs
    (lookupEncounterCardDef b)
    c
    d
    e
    f
    a

instance HasVictoryPoints Location where
  getVictoryPoints l =
    let LocationAttrs { locationClues } = toAttrs l
    in if locationClues == 0 then l ^. defL . victoryPointsL  else Nothing

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

instance HasId LocationSymbol env Location where
  getId = getId . toAttrs

instance HasId (Maybe LocationId) env (Direction, Location) where
  getId (dir, location) = getId (dir, toAttrs location)

getLocationName :: Location -> LocationName
getLocationName l = if locationRevealed attrs
  then LocationName $ l ^. defL . nameL
  else locationUnrevealedName attrs
  where attrs = toAttrs l

lookupLocationStub :: CardCode -> Location
lookupLocationStub = ($ LocationId (CardId nil)) . lookupLocation

lookupLocation :: CardCode -> (LocationId -> Location)
lookupLocation lid =
  fromJustNote ("Unknown location: " <> show lid) $ lookup lid allLocations

allLocations :: HashMap CardCode (LocationId -> Location)
allLocations = mapFromList
  [ ("01111", Study' . study)
  , ("01112", Hallway' . hallway)
  , ("01113", Attic' . attic)
  , ("01114", Cellar' . cellar)
  , ("01115", Parlor' . parlor)
  , ("01124", YourHouse' . yourHouse)
  , ("01125", Rivertown' . rivertown)
  , ("01126", SouthsideHistoricalSociety' . southsideHistoricalSociety)
  , ("01127", SouthsideMasBoardingHouse' . southsideMasBoardingHouse)
  , ("01128", StMarysHospital' . stMarysHospital)
  , ("01129", MiskatonicUniversity' . miskatonicUniversity)
  , ("01130", DowntownFirstBankOfArkham' . downtownFirstBankOfArkham)
  , ("01131", DowntownArkhamAsylum' . downtownArkhamAsylum)
  , ("01132", Easttown' . easttown)
  , ("01133", Graveyard' . graveyard)
  , ("01134", Northside' . northside)
  , ("01149", MainPath' . mainPath)
  , ("01150", ArkhamWoodsUnhallowedGround' . arkhamWoodsUnhallowedGround)
  , ("01151", ArkhamWoodsTwistingPaths' . arkhamWoodsTwistingPaths)
  , ("01152", ArkhamWoodsOldHouse' . arkhamWoodsOldHouse)
  , ("01153", ArkhamWoodsCliffside' . arkhamWoodsCliffside)
  , ("01154", ArkhamWoodsTangledThicket' . arkhamWoodsTangledThicket)
  , ("01155", ArkhamWoodsQuietGlade' . arkhamWoodsQuietGlade)
  , ("01156", RitualSite' . ritualSite)
  , ("02048", MiskatonicQuad' . miskatonicQuad)
  , ("02049", HumanitiesBuilding' . humanitiesBuilding)
  , ("02050", OrneLibrary' . orneLibrary)
  , ("02051", StudentUnion' . studentUnion)
  , ("02052", Dormitories' . dormitories)
  , ("02053", AdministrationBuilding' . administrationBuilding)
  , ( "02054"
    , FacultyOfficesTheNightIsStillYoung' . facultyOfficesTheNightIsStillYoung
    )
  , ("02055", FacultyOfficesTheHourIsLate' . facultyOfficesTheHourIsLate)
  , ("02056", ScienceBuilding' . scienceBuilding)
  , ("02057", AlchemyLabs' . alchemyLabs)
  , ("02070", LaBellaLuna' . laBellaLuna)
  , ("02071", CloverClubLounge' . cloverClubLounge)
  , ("02072", CloverClubBar' . cloverClubBar)
  , ("02073", CloverClubCardroom' . cloverClubCardroom)
  , ("02074", DarkenedHall' . darkenedHall)
  , ("02075", ArtGallery' . artGallery)
  , ("02076", VipArea' . vipArea)
  , ("02077", BackAlley' . backAlley)
  , ("02126", MuseumEntrance' . museumEntrance)
  , ("02127", MuseumHalls' . museumHalls)
  , ("02128", SecurityOffice_128' . securityOffice_128)
  , ("02129", SecurityOffice_129' . securityOffice_129)
  , ("02130", AdministrationOffice_130' . administrationOffice_130)
  , ("02131", AdministrationOffice_131' . administrationOffice_131)
  , ("02132", ExhibitHallAthabaskanExhibit' . exhibitHallAthabaskanExhibit)
  , ("02133", ExhibitHallMedusaExhibit' . exhibitHallMedusaExhibit)
  , ("02134", ExhibitHallNatureExhibit' . exhibitHallNatureExhibit)
  , ("02135", ExhibitHallEgyptianExhibit' . exhibitHallEgyptianExhibit)
  , ("02136", ExhibitHallHallOfTheDead' . exhibitHallHallOfTheDead)
  , ("02137", ExhibitHallRestrictedHall' . exhibitHallRestrictedHall)
  , ("02167", PassengerCar_167' . passengerCar_167)
  , ("02168", PassengerCar_168' . passengerCar_168)
  , ("02169", PassengerCar_169' . passengerCar_169)
  , ("02170", PassengerCar_170' . passengerCar_170)
  , ("02171", PassengerCar_171' . passengerCar_171)
  , ("02172", SleepingCar' . sleepingCar)
  , ("02173", DiningCar' . diningCar)
  , ("02174", ParlorCar' . parlorCar)
  , ("02175", EngineCar_175' . engineCar_175)
  , ("02176", EngineCar_176' . engineCar_176)
  , ("02177", EngineCar_177' . engineCar_177)
  , ("02201", VillageCommons' . villageCommons)
  , ("02202", BishopsBrook_202' . bishopsBrook_202)
  , ("02203", BishopsBrook_203' . bishopsBrook_203)
  , ("02204", BurnedRuins_204' . burnedRuins_204)
  , ("02205", BurnedRuins_205' . burnedRuins_205)
  , ("02206", OsbornsGeneralStore_206' . osbornsGeneralStore_206)
  , ("02207", OsbornsGeneralStore_207' . osbornsGeneralStore_207)
  , ("02208", CongregationalChurch_208' . congregationalChurch_208)
  , ("02209", CongregationalChurch_209' . congregationalChurch_209)
  , ("02210", HouseInTheReeds_210' . houseInTheReeds_210)
  , ("02211", HouseInTheReeds_211' . houseInTheReeds_211)
  , ("02212", Schoolhouse_212' . schoolhouse_212)
  , ("02213", Schoolhouse_213' . schoolhouse_213)
  , ("02214", TheHiddenChamber' . theHiddenChamber)
  , ("02242", DunwichVillage_242' . dunwichVillage_242)
  , ("02243", DunwichVillage_243' . dunwichVillage_243)
  , ("02244", ColdSpringGlen_244' . coldSpringGlen_244)
  , ("02245", ColdSpringGlen_245' . coldSpringGlen_245)
  , ("02246", TenAcreMeadow_246' . tenAcreMeadow_246)
  , ("02247", TenAcreMeadow_247' . tenAcreMeadow_247)
  , ("02248", BlastedHeath_248' . blastedHeath_248)
  , ("02249", BlastedHeath_249' . blastedHeath_249)
  , ("02250", WhateleyRuins_250' . whateleyRuins_250)
  , ("02251", WhateleyRuins_251' . whateleyRuins_251)
  , ("02252", DevilsHopYard_252' . devilsHopYard_252)
  , ("02253", DevilsHopYard_253' . devilsHopYard_253)
  , ("02282", BaseOfTheHill' . baseOfTheHill)
  , ("02283", AscendingPath' . ascendingPath)
  , ("02284", SentinelPeak' . sentinelPeak)
  , ("02285", SlaughteredWoods' . slaugteredWoods)
  , ("02286", EerieGlade' . eerieGlade)
  , ("02287", DestroyedPath' . destroyedPath)
  , ("02288", FrozenSpring' . frozenSpring)
  , ("02289", DimensionalGap' . dimensionalGap)
  , ("02290", ATearInThePath' . aTearInThePath)
  , ("02291", UprootedWoods' . uprootedWoods)
  , ("02292", LostMemories' . lostMemories)
  , ("02320", AnotherDimension' . anotherDimension)
  , ("02321", TheEdgeOfTheUniverse' . theEdgeOfTheUniverse)
  , ("02322", TearThroughTime' . tearThroughTime)
  , ("02324", TearThroughSpace' . tearThroughSpace)
  , ("02325", PrismaticCascade' . prismaticCascade)
  , ("02326", EndlessBridge' . endlessBridge)
  , ("02327", StepsOfYhagharl' . stepsOfYhagharl)
  , ("02328", DimensionalDoorway' . dimensionalDoorway)
  , ("50013", StudyAberrantGateway' . studyAberrantGateway)
  , ("50014", GuestHall' . guestHall)
  , ("50015", Bedroom' . bedroom)
  , ("50016", Bathroom' . bathroom)
  , ("50017", HoleInTheWall' . holeInTheWall)
  , ("50018", ReturnToAttic' . returnToAttic)
  , ("50019", FarAboveYourHouse' . farAboveYourHouse)
  , ("50020", ReturnToCellar' . returnToCellar)
  , ("50021", DeepBelowYourHouse' . deepBelowYourHouse)
  , ("50027", EasttownArkhamPoliceStation' . easttownArkhamPoliceStation)
  , ("50028", NorthsideTrainStation' . northsideTrainStation)
  , ( "50029"
    , MiskatonicUniversityMiskatonicMuseum'
      . miskatonicUniversityMiskatonicMuseum
    )
  , ("50030", RivertownAbandonedWarehouse' . rivertownAbandonedWarehouse)
  , ("50033", ArkhamWoodsGreatWillow' . arkhamWoodsGreatWillow)
  , ("50034", ArkhamWoodsLakeside' . arkhamWoodsLakeside)
  , ( "50035"
    , ArkhamWoodsCorpseRiddenClearing' . arkhamWoodsCorpseRiddenClearing
    )
  , ("50036", ArkhamWoodsWoodenBridge' . arkhamWoodsWoodenBridge)
  , ("50037", RitualSite' . ritualSite)
  , ("81007", CursedShores' . cursedShores)
  , ("81008", GardenDistrict' . gardenDistrict)
  , ("81009", Broadmoor' . broadmoor)
  , ("81010", BrackishWaters' . brackishWaters)
  , ("81011", AudubonPark' . audubonPark)
  , ("81012", FauborgMarigny' . fauborgMarigny)
  , ("81013", ForgottenMarsh' . forgottenMarsh)
  , ("81014", TrappersCabin' . trappersCabin)
  , ("81015", TwistedUnderbrush' . twistedUnderbrush)
  , ("81016", FoulSwamp' . foulSwamp)
  , ("81017", RitualGrounds' . ritualGrounds)
  , ("81018", OvergrownCairns' . overgrownCairns)
  ]

isEmptyLocation :: Location -> Bool
isEmptyLocation l = null enemies' && null investigators'
 where
  enemies' = locationEnemies $ toAttrs l
  investigators' = locationInvestigators $ toAttrs l

isRevealed :: Location -> Bool
isRevealed = locationRevealed . toAttrs
