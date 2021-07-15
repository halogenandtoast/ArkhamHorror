module Arkham.Types.Location
  ( module Arkham.Types.Location
  ) where

import Arkham.Prelude

import Arkham.EncounterCard
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Cards
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Query
import Data.UUID (nil)

createLocation :: IsCard a => a -> Location
createLocation a = lookupLocation (toCardCode a) (LocationId $ toCardId a)

toLocationSymbol :: Location -> LocationSymbol
toLocationSymbol = locationSymbol . toAttrs

data Location
  = BaseLocation' BaseLocation
  | ATearInThePath' ATearInThePath
  | AccademiaBridge' AccademiaBridge
  | AdministrationBuilding' AdministrationBuilding
  | AdministrationOffice_130' AdministrationOffice_130
  | AdministrationOffice_131' AdministrationOffice_131
  | AlchemyLabs' AlchemyLabs
  | AnotherDimension' AnotherDimension
  | ArkhamWoodsCliffside' ArkhamWoodsCliffside
  | ArkhamWoodsCorpseRiddenClearing' ArkhamWoodsCorpseRiddenClearing
  | ArkhamWoodsGreatWillow' ArkhamWoodsGreatWillow
  | ArkhamWoodsLakeside' ArkhamWoodsLakeside
  | ArkhamWoodsOldHouse' ArkhamWoodsOldHouse
  | ArkhamWoodsQuietGlade' ArkhamWoodsQuietGlade
  | ArkhamWoodsTangledThicket' ArkhamWoodsTangledThicket
  | ArkhamWoodsTwistingPaths' ArkhamWoodsTwistingPaths
  | ArkhamWoodsUnhallowedGround' ArkhamWoodsUnhallowedGround
  | ArkhamWoodsWoodenBridge' ArkhamWoodsWoodenBridge
  | ArtGallery' ArtGallery
  | AscendingPath' AscendingPath
  | Attic' Attic
  | AudubonPark' AudubonPark
  | BackAlley' BackAlley
  | BaseOfTheHill' BaseOfTheHill
  | Bathroom' Bathroom
  | Bedroom' Bedroom
  | BishopsBrook_202' BishopsBrook_202
  | BishopsBrook_203' BishopsBrook_203
  | BlastedHeath_248' BlastedHeath_248
  | BlastedHeath_249' BlastedHeath_249
  | BrackishWaters' BrackishWaters
  | BridgeOfSighs' BridgeOfSighs
  | Broadmoor' Broadmoor
  | BurnedRuins_204' BurnedRuins_204
  | BurnedRuins_205' BurnedRuins_205
  | CanalSide' CanalSide
  | Cellar' Cellar
  | CloverClubBar' CloverClubBar
  | CloverClubCardroom' CloverClubCardroom
  | CloverClubLounge' CloverClubLounge
  | ColdSpringGlen_244' ColdSpringGlen_244
  | ColdSpringGlen_245' ColdSpringGlen_245
  | CongregationalChurch_208' CongregationalChurch_208
  | CongregationalChurch_209' CongregationalChurch_209
  | CursedShores' CursedShores
  | DarkenedHall' DarkenedHall
  | DeepBelowYourHouse' DeepBelowYourHouse
  | DestroyedPath' DestroyedPath
  | DevilsHopYard_252' DevilsHopYard_252
  | DevilsHopYard_253' DevilsHopYard_253
  | DimensionalDoorway' DimensionalDoorway
  | DimensionalGap' DimensionalGap
  | DiningCar' DiningCar
  | Dormitories' Dormitories
  | DowntownArkhamAsylum' DowntownArkhamAsylum
  | DowntownFirstBankOfArkham' DowntownFirstBankOfArkham
  | DunwichVillage_242' DunwichVillage_242
  | DunwichVillage_243' DunwichVillage_243
  | Easttown' Easttown
  | EasttownArkhamPoliceStation' EasttownArkhamPoliceStation
  | EerieGlade' EerieGlade
  | EndlessBridge' EndlessBridge
  | EngineCar_175' EngineCar_175
  | EngineCar_176' EngineCar_176
  | EngineCar_177' EngineCar_177
  | ExhibitHallAthabaskanExhibit' ExhibitHallAthabaskanExhibit
  | ExhibitHallEgyptianExhibit' ExhibitHallEgyptianExhibit
  | ExhibitHallHallOfTheDead' ExhibitHallHallOfTheDead
  | ExhibitHallMedusaExhibit' ExhibitHallMedusaExhibit
  | ExhibitHallNatureExhibit' ExhibitHallNatureExhibit
  | ExhibitHallRestrictedHall' ExhibitHallRestrictedHall
  | FacultyOfficesTheHourIsLate' FacultyOfficesTheHourIsLate
  | FacultyOfficesTheNightIsStillYoung' FacultyOfficesTheNightIsStillYoung
  | FarAboveYourHouse' FarAboveYourHouse
  | FauborgMarigny' FauborgMarigny
  | FloodedSquare' FloodedSquare
  | ForgottenMarsh' ForgottenMarsh
  | FoulSwamp' FoulSwamp
  | FrozenSpring' FrozenSpring
  | GardenDistrict' GardenDistrict
  | Graveyard' Graveyard
  | GuestHall' GuestHall
  | Hallway' Hallway
  | HoleInTheWall' HoleInTheWall
  | HouseInTheReeds_210' HouseInTheReeds_210
  | HouseInTheReeds_211' HouseInTheReeds_211
  | HumanitiesBuilding' HumanitiesBuilding
  | LaBellaLuna' LaBellaLuna
  | LostMemories' LostMemories
  | MainPath' MainPath
  | MiskatonicQuad' MiskatonicQuad
  | MiskatonicUniversity' MiskatonicUniversity
  | MiskatonicUniversityMiskatonicMuseum' MiskatonicUniversityMiskatonicMuseum
  | MuseumEntrance' MuseumEntrance
  | MuseumHalls' MuseumHalls
  | Northside' Northside
  | NorthsideTrainStation' NorthsideTrainStation
  | OrneLibrary' OrneLibrary
  | OsbornsGeneralStore_206' OsbornsGeneralStore_206
  | OsbornsGeneralStore_207' OsbornsGeneralStore_207
  | OvergrownCairns' OvergrownCairns
  | Parlor' Parlor
  | ParlorCar' ParlorCar
  | PassengerCar_167' PassengerCar_167
  | PassengerCar_168' PassengerCar_168
  | PassengerCar_169' PassengerCar_169
  | PassengerCar_170' PassengerCar_170
  | PassengerCar_171' PassengerCar_171
  | PrismaticCascade' PrismaticCascade
  | ReturnToAttic' ReturnToAttic
  | ReturnToCellar' ReturnToCellar
  | RialtoBridge' RialtoBridge
  | RitualGrounds' RitualGrounds
  | RitualSite' RitualSite
  | Rivertown' Rivertown
  | RivertownAbandonedWarehouse' RivertownAbandonedWarehouse
  | SanMarcoBasilica' SanMarcoBasilica
  | Schoolhouse_212' Schoolhouse_212
  | Schoolhouse_213' Schoolhouse_213
  | ScienceBuilding' ScienceBuilding
  | SecurityOffice_128' SecurityOffice_128
  | SecurityOffice_129' SecurityOffice_129
  | SentinelPeak' SentinelPeak
  | SlaughteredWoods' SlaughteredWoods
  | SleepingCar' SleepingCar
  | SouthsideHistoricalSociety' SouthsideHistoricalSociety
  | SouthsideMasBoardingHouse' SouthsideMasBoardingHouse
  | StMarysHospital' StMarysHospital
  | StepsOfYhagharl' StepsOfYhagharl
  | StreetsOfVenice' StreetsOfVenice
  | StudentUnion' StudentUnion
  | Study' Study
  | StudyAberrantGateway' StudyAberrantGateway
  | TearThroughSpace' TearThroughSpace
  | TearThroughTime' TearThroughTime
  | TenAcreMeadow_246' TenAcreMeadow_246
  | TenAcreMeadow_247' TenAcreMeadow_247
  | TheEdgeOfTheUniverse' TheEdgeOfTheUniverse
  | TheGuardian' TheGuardian
  | TheHiddenChamber' TheHiddenChamber
  | TrappersCabin' TrappersCabin
  | TwistedUnderbrush' TwistedUnderbrush
  | UprootedWoods' UprootedWoods
  | VenetianGarden' VenetianGarden
  | VillageCommons' VillageCommons
  | VipArea' VipArea
  | WhateleyRuins_250' WhateleyRuins_250
  | WhateleyRuins_251' WhateleyRuins_251
  | YourHouse' YourHouse
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance IsCard Location where
  toCardId = toCardId . toAttrs

deriving anyclass instance ActionRunner env => HasActions env Location

instance
  ( HasPhase env
  , HasCount CardCount env InvestigatorId
  , HasCount ClueCount env LocationId
  , HasCount ResourceCount env InvestigatorId
  , HasId (Maybe StoryEnemyId) env CardCode
  , HasId (Maybe StoryAssetId) env CardCode
  )
  => HasModifiersFor env Location where
  getModifiersFor = genericGetModifiersFor

instance (HasSet UnengagedEnemyId env LocationId, LocationRunner env) => RunMessage env Location where
  runMessage msg l = do
    modifiers' <- getModifiersFor (toSource l) (toTarget l) ()
    let msg' = if any isBlank modifiers' then Blanked msg else msg
    defaultRunMessage msg' l

instance Entity Location where
  type EntityId Location = LocationId
  type EntityAttrs Location = LocationAttrs

instance Named Location where
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
  toCardDef = toCardDef . toAttrs

instance HasModifiersFor env BaseLocation

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
  -> (CardDef -> CardDef)
  -> Location
baseLocation a b c d e f attrsF defF = BaseLocation' $ cbCardBuilder
  (locationWith BaseLocation (defF $ lookupEncounterCardDef b) c d e f attrsF)
  a

instance HasVictoryPoints Location where
  getVictoryPoints l =
    let LocationAttrs { locationClues } = toAttrs l
    in if locationClues == 0 then cdVictoryPoints (toCardDef l) else Nothing

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
  getId (dir, location') = getId (dir, toAttrs location')

getLocationName :: Location -> LocationName
getLocationName l = if locationRevealed attrs
  then LocationName $ cdName (toCardDef l)
  else locationUnrevealedName attrs
  where attrs = toAttrs l

lookupLocationStub :: CardCode -> Location
lookupLocationStub = ($ LocationId (CardId nil)) . lookupLocation

lookupLocation :: CardCode -> (LocationId -> Location)
lookupLocation lid =
  fromJustNote ("Unknown location: " <> show lid) $ lookup lid allLocations

allLocations :: HashMap CardCode (LocationId -> Location)
allLocations = mapFromList $ map
  (cbCardCode &&& cbCardBuilder)
  [ CardBuilder
    { cbCardCode = "location"
    , cbCardBuilder = \lid ->
      baseLocation lid "location" 0 (Static 0) NoSymbol [] id id
    }
  , ATearInThePath' <$> aTearInThePath
  , AccademiaBridge' <$> accademiaBridge
  , AdministrationBuilding' <$> administrationBuilding
  , AdministrationOffice_130' <$> administrationOffice_130
  , AdministrationOffice_131' <$> administrationOffice_131
  , AlchemyLabs' <$> alchemyLabs
  , AnotherDimension' <$> anotherDimension
  , ArkhamWoodsCliffside' <$> arkhamWoodsCliffside
  , ArkhamWoodsCorpseRiddenClearing' <$> arkhamWoodsCorpseRiddenClearing
  , ArkhamWoodsGreatWillow' <$> arkhamWoodsGreatWillow
  , ArkhamWoodsLakeside' <$> arkhamWoodsLakeside
  , ArkhamWoodsOldHouse' <$> arkhamWoodsOldHouse
  , ArkhamWoodsQuietGlade' <$> arkhamWoodsQuietGlade
  , ArkhamWoodsTangledThicket' <$> arkhamWoodsTangledThicket
  , ArkhamWoodsTwistingPaths' <$> arkhamWoodsTwistingPaths
  , ArkhamWoodsUnhallowedGround' <$> arkhamWoodsUnhallowedGround
  , ArkhamWoodsWoodenBridge' <$> arkhamWoodsWoodenBridge
  , ArtGallery' <$> artGallery
  , AscendingPath' <$> ascendingPath
  , Attic' <$> attic
  , AudubonPark' <$> audubonPark
  , BackAlley' <$> backAlley
  , BaseOfTheHill' <$> baseOfTheHill
  , Bathroom' <$> bathroom
  , Bedroom' <$> bedroom
  , BishopsBrook_202' <$> bishopsBrook_202
  , BishopsBrook_203' <$> bishopsBrook_203
  , BlastedHeath_248' <$> blastedHeath_248
  , BlastedHeath_249' <$> blastedHeath_249
  , BrackishWaters' <$> brackishWaters
  , BridgeOfSighs' <$> bridgeOfSighs
  , Broadmoor' <$> broadmoor
  , BurnedRuins_204' <$> burnedRuins_204
  , BurnedRuins_205' <$> burnedRuins_205
  , CanalSide' <$> canalSide
  , Cellar' <$> cellar
  , CloverClubBar' <$> cloverClubBar
  , CloverClubCardroom' <$> cloverClubCardroom
  , CloverClubLounge' <$> cloverClubLounge
  , ColdSpringGlen_244' <$> coldSpringGlen_244
  , ColdSpringGlen_245' <$> coldSpringGlen_245
  , CongregationalChurch_208' <$> congregationalChurch_208
  , CongregationalChurch_209' <$> congregationalChurch_209
  , CursedShores' <$> cursedShores
  , DarkenedHall' <$> darkenedHall
  , DeepBelowYourHouse' <$> deepBelowYourHouse
  , DestroyedPath' <$> destroyedPath
  , DevilsHopYard_252' <$> devilsHopYard_252
  , DevilsHopYard_253' <$> devilsHopYard_253
  , DimensionalDoorway' <$> dimensionalDoorway
  , DimensionalGap' <$> dimensionalGap
  , DiningCar' <$> diningCar
  , Dormitories' <$> dormitories
  , DowntownArkhamAsylum' <$> downtownArkhamAsylum
  , DowntownFirstBankOfArkham' <$> downtownFirstBankOfArkham
  , DunwichVillage_242' <$> dunwichVillage_242
  , DunwichVillage_243' <$> dunwichVillage_243
  , Easttown' <$> easttown
  , EasttownArkhamPoliceStation' <$> easttownArkhamPoliceStation
  , EerieGlade' <$> eerieGlade
  , EndlessBridge' <$> endlessBridge
  , EngineCar_175' <$> engineCar_175
  , EngineCar_176' <$> engineCar_176
  , EngineCar_177' <$> engineCar_177
  , ExhibitHallAthabaskanExhibit' <$> exhibitHallAthabaskanExhibit
  , ExhibitHallEgyptianExhibit' <$> exhibitHallEgyptianExhibit
  , ExhibitHallHallOfTheDead' <$> exhibitHallHallOfTheDead
  , ExhibitHallMedusaExhibit' <$> exhibitHallMedusaExhibit
  , ExhibitHallNatureExhibit' <$> exhibitHallNatureExhibit
  , ExhibitHallRestrictedHall' <$> exhibitHallRestrictedHall
  , FacultyOfficesTheHourIsLate' <$> facultyOfficesTheHourIsLate
  , FacultyOfficesTheNightIsStillYoung' <$> facultyOfficesTheNightIsStillYoung
  , FarAboveYourHouse' <$> farAboveYourHouse
  , FauborgMarigny' <$> fauborgMarigny
  , FloodedSquare' <$> floodedSquare
  , ForgottenMarsh' <$> forgottenMarsh
  , FoulSwamp' <$> foulSwamp
  , FrozenSpring' <$> frozenSpring
  , GardenDistrict' <$> gardenDistrict
  , Graveyard' <$> graveyard
  , GuestHall' <$> guestHall
  , Hallway' <$> hallway
  , HoleInTheWall' <$> holeInTheWall
  , HouseInTheReeds_210' <$> houseInTheReeds_210
  , HouseInTheReeds_211' <$> houseInTheReeds_211
  , HumanitiesBuilding' <$> humanitiesBuilding
  , LaBellaLuna' <$> laBellaLuna
  , LostMemories' <$> lostMemories
  , MainPath' <$> mainPath
  , MiskatonicQuad' <$> miskatonicQuad
  , MiskatonicUniversity' <$> miskatonicUniversity
  , MiskatonicUniversityMiskatonicMuseum'
    <$> miskatonicUniversityMiskatonicMuseum
  , MuseumEntrance' <$> museumEntrance
  , MuseumHalls' <$> museumHalls
  , Northside' <$> northside
  , NorthsideTrainStation' <$> northsideTrainStation
  , OrneLibrary' <$> orneLibrary
  , OsbornsGeneralStore_206' <$> osbornsGeneralStore_206
  , OsbornsGeneralStore_207' <$> osbornsGeneralStore_207
  , OvergrownCairns' <$> overgrownCairns
  , Parlor' <$> parlor
  , ParlorCar' <$> parlorCar
  , PassengerCar_167' <$> passengerCar_167
  , PassengerCar_168' <$> passengerCar_168
  , PassengerCar_169' <$> passengerCar_169
  , PassengerCar_170' <$> passengerCar_170
  , PassengerCar_171' <$> passengerCar_171
  , PrismaticCascade' <$> prismaticCascade
  , ReturnToAttic' <$> returnToAttic
  , ReturnToCellar' <$> returnToCellar
  , RialtoBridge' <$> rialtoBridge
  , RitualGrounds' <$> ritualGrounds
  , RitualSite' <$> ritualSite
  , Rivertown' <$> rivertown
  , RivertownAbandonedWarehouse' <$> rivertownAbandonedWarehouse
  , SanMarcoBasilica' <$> sanMarcoBasilica
  , Schoolhouse_212' <$> schoolhouse_212
  , Schoolhouse_213' <$> schoolhouse_213
  , ScienceBuilding' <$> scienceBuilding
  , SecurityOffice_128' <$> securityOffice_128
  , SecurityOffice_129' <$> securityOffice_129
  , SentinelPeak' <$> sentinelPeak
  , SlaughteredWoods' <$> slaugteredWoods
  , SleepingCar' <$> sleepingCar
  , SouthsideHistoricalSociety' <$> southsideHistoricalSociety
  , SouthsideMasBoardingHouse' <$> southsideMasBoardingHouse
  , StMarysHospital' <$> stMarysHospital
  , StepsOfYhagharl' <$> stepsOfYhagharl
  , StreetsOfVenice' <$> streetsOfVenice
  , StudentUnion' <$> studentUnion
  , Study' <$> study
  , StudyAberrantGateway' <$> studyAberrantGateway
  , TearThroughSpace' <$> tearThroughSpace
  , TearThroughTime' <$> tearThroughTime
  , TenAcreMeadow_246' <$> tenAcreMeadow_246
  , TenAcreMeadow_247' <$> tenAcreMeadow_247
  , TheEdgeOfTheUniverse' <$> theEdgeOfTheUniverse
  , TheGuardian' <$> theGuardian
  , TheHiddenChamber' <$> theHiddenChamber
  , TrappersCabin' <$> trappersCabin
  , TwistedUnderbrush' <$> twistedUnderbrush
  , UprootedWoods' <$> uprootedWoods
  , VenetianGarden' <$> venetianGarden
  , VillageCommons' <$> villageCommons
  , VipArea' <$> vipArea
  , WhateleyRuins_250' <$> whateleyRuins_250
  , WhateleyRuins_251' <$> whateleyRuins_251
  , YourHouse' <$> yourHouse
  ]

isEmptyLocation :: Location -> Bool
isEmptyLocation l = null enemies' && null investigators'
 where
  enemies' = locationEnemies $ toAttrs l
  investigators' = locationInvestigators $ toAttrs l

isRevealed :: Location -> Bool
isRevealed = locationRevealed . toAttrs
