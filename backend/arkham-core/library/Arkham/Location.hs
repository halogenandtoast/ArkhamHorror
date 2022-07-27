module Arkham.Location
  ( module Arkham.Location
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Card.Id
import Arkham.Classes
import Arkham.Id
import Arkham.Helpers.Modifiers
import Arkham.Label qualified as L
import Arkham.Location.Data (LocationData, HasLocationData(toLocationData))
import Arkham.Location.Locations
import Arkham.Location.Runner
import Arkham.Message
import Arkham.Name
import Data.UUID (nil)
import Data.Typeable
import Data.Aeson.Types

data Location = forall a. IsLocation a => Location a

instance Eq Location where
  (Location (a :: a)) == (Location (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Location where
  show (Location a) = show a

instance ToJSON Location where
  toJSON (Location a) = toJSON a

createLocation :: IsCard a => a -> Location
createLocation a = lookupLocation (toCardCode a) (LocationId $ toCardId a)

toLocationSymbol :: Location -> LocationSymbol
toLocationSymbol = locationSymbol . toAttrs

toLocationLabel :: Location -> L.Label
toLocationLabel = L.Label . locationLabel . toAttrs

instance HasCardCode Location where
  toCardCode = toCardCode . toAttrs

instance HasAbilities Location where
  getAbilities (Location a) = getAbilities a

instance HasModifiersFor Location where
  getModifiersFor source target (Location a) = getModifiersFor source target a

instance RunMessage Location where
  runMessage msg x@(Location l) = do
    modifiers' <- getModifiers (toSource x) (toTarget x)
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    Location <$> runMessage msg' l

instance Entity Location where
  type EntityId Location = LocationId
  type EntityAttrs Location = LocationAttrs
  toId = toId . toAttrs
  toAttrs (Location l) = toAttrs l
  overAttrs f (Location a) = Location $ overAttrs f a

instance Named Location where
  toName = toName . toAttrs

instance Named (Unrevealed Location) where
  toName (Unrevealed l) = toName . Unrevealed $ toAttrs l

instance TargetEntity Location where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Location where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

lookupLocationStub :: CardCode -> Location
lookupLocationStub = ($ LocationId (CardId nil)) . lookupLocation

lookupLocation :: CardCode -> (LocationId -> Location)
lookupLocation lid =
  fromJustNote ("Unknown location: " <> show lid) $ lookup lid allLocations

isEmptyLocation :: Location -> Bool
isEmptyLocation =
  and . sequence [noInvestigatorsAtLocation, noEnemiesAtLocation]

noInvestigatorsAtLocation :: Location -> Bool
noInvestigatorsAtLocation l = null investigators'
  where investigators' = locationInvestigators $ toAttrs l

noEnemiesAtLocation :: Location -> Bool
noEnemiesAtLocation l = null enemies'
  where enemies' = locationEnemies $ toAttrs l

isRevealed :: Location -> Bool
isRevealed = locationRevealed . toAttrs

instance FromJSON Location where
  parseJSON v = flip (withObject "Location") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withLocationCardCode cCode $ _
      -- \(z :: (LocationAttrs -> a) -> Parser Location) -> Location . z  <$> parseJSON v

getLocationData :: CardCode -> LocationData
getLocationData ccode = withLocationCardCode ccode $ \(_ :: p k) -> toLocationData @k

withLocationCardCode :: CardCode -> (forall a. (HasLocationData a, IsLocation a) => proxy a -> r) -> r
withLocationCardCode ccode f = case ccode of
  -- Night of the Zealot
  -- The Gathering
  "01111" -> f Study
  "01112" -> f Hallway
  "01113" -> f Attic
  "01114" -> f Cellar
  "01115" -> f Parlor
  -- The Midnight Masks
  "01124" -> f YourHouse
  "01125" -> f Rivertown
  "01126" -> f SouthsideHistoricalSociety
  "01127" -> f SouthsideMasBoardingHouse
  "01128" -> f StMarysHospital
  "01129" -> f MiskatonicUniversity
  "01130" -> f DowntownFirstBankOfArkham
  "01131" -> f DowntownArkhamAsylum
  "01132" -> f Easttown
  "01133" -> f Graveyard
  "01134" -> f Northside
  -- The Devourer Below
  "01149" -> f MainPath
  "01150" -> f ArkhamWoodsUnhallowedGround
  "01151" -> f ArkhamWoodsTwistingPaths
  "01152" -> f ArkhamWoodsOldHouse
  "01153" -> f ArkhamWoodsCliffside
  "01154" -> f ArkhamWoodsTangledThicket
  "01155" -> f ArkhamWoodsQuietGlade
  "01156" -> f RitualSite
  -- The Dunwich Legacy
  -- Extracurricular Activity
  "02048" -> f MiskatonicQuad
  "02049" -> f HumanitiesBuilding
  "02050" -> f OrneLibrary
  "02051" -> f StudentUnion
  "02052" -> f Dormitories
  "02053" -> f AdministrationBuilding
  "02054" -> f FacultyOfficesTheNightIsStillYoung
  "02055" -> f FacultyOfficesTheHourIsLate
  "02056" -> f ScienceBuilding
  "02057" -> f AlchemyLabs
  -- The House Always Wins
  "02070" -> f LaBellaLuna
  "02071" -> f CloverClubLounge
  "02072" -> f CloverClubBar
  "02073" -> f CloverClubCardroom
  "02074" -> f DarkenedHall
  "02075" -> f ArtGallery
  "02076" -> f VipArea
  "02077" -> f BackAlley
  -- The Miskatonic Museum
  "02126" -> f MuseumEntrance
  "02127" -> f MuseumHalls
  "02128" -> f SecurityOffice_128
  "02129" -> f SecurityOffice_129
  "02130" -> f AdministrationOffice_130
  "02131" -> f AdministrationOffice_131
  "02132" -> f ExhibitHallAthabaskanExhibit
  "02133" -> f ExhibitHallMedusaExhibit
  "02134" -> f ExhibitHallNatureExhibit
  "02135" -> f ExhibitHallEgyptianExhibit
  "02136" -> f ExhibitHallHallOfTheDead
  "02137" -> f ExhibitHallRestrictedHall
  -- The Essex County Express
  "02167" -> f PassengerCar_167
  "02168" -> f PassengerCar_168
  "02169" -> f PassengerCar_169
  "02170" -> f PassengerCar_170
  "02171" -> f PassengerCar_171
  "02172" -> f SleepingCar
  "02173" -> f DiningCar
  "02174" -> f ParlorCar
  "02175" -> f EngineCar_175
  "02176" -> f EngineCar_176
  "02177" -> f EngineCar_177
  -- Blood on the Altar
  "02201" -> f VillageCommons
  "02202" -> f BishopsBrook_202
  "02203" -> f BishopsBrook_203
  "02204" -> f BurnedRuins_204
  "02205" -> f BurnedRuins_205
  "02206" -> f OsbornsGeneralStore_206
  "02207" -> f OsbornsGeneralStore_207
  "02208" -> f CongregationalChurch_208
  "02209" -> f CongregationalChurch_209
  "02210" -> f HouseInTheReeds_210
  "02211" -> f HouseInTheReeds_211
  "02212" -> f Schoolhouse_212
  "02213" -> f Schoolhouse_213
  "02214" -> f TheHiddenChamber
  -- Undimensioned and Unseen
  "02242" -> f DunwichVillage_242
  "02243" -> f DunwichVillage_243
  "02244" -> f ColdSpringGlen_244
  "02245" -> f ColdSpringGlen_245
  "02246" -> f TenAcreMeadow_246
  "02247" -> f TenAcreMeadow_247
  "02248" -> f BlastedHeath_248
  "02249" -> f BlastedHeath_249
  "02250" -> f WhateleyRuins_250
  "02251" -> f WhateleyRuins_251
  "02252" -> f DevilsHopYard_252
  "02253" -> f DevilsHopYard_253
  -- Where Doom Awaits
  "02282" -> f BaseOfTheHill
  "02283" -> f AscendingPath
  "02284" -> f SentinelPeak
  "02285" -> f SlaughteredWoods
  "02286" -> f EerieGlade
  "02287" -> f DestroyedPath
  "02288" -> f FrozenSpring
  "02289" -> f DimensionalGap
  "02290" -> f ATearInThePath
  "02291" -> f UprootedWoods
  "02292" -> f LostMemories
  -- Lost in Time and Space
  "02310" -> f AnotherDimension
  "02311" -> f TheEdgeOfTheUniverse
  "02312" -> f TearThroughTime
  "02313" -> f TearThroughSpace
  "02314" -> f PrismaticCascade
  "02315" -> f EndlessBridge
  "02316" -> f StepsOfYhagharl
  "02317" -> f DimensionalDoorway
  -- The Path To Carcosa
  -- Curtain Call
  "03049" -> f Theatre
  "03050" -> f Lobby
  "03051" -> f Balcony
  "03052" -> f Backstage
  "03053" -> f LightingBox
  "03054" -> f BoxOffice
  "03055" -> f GreenRoom
  "03056" -> f DressingRoom
  "03057" -> f RehearsalRoom
  "03058" -> f TrapRoom
  -- The Last King
  "03070" -> f Foyer
  "03071" -> f Ballroom
  "03072" -> f LivingRoom
  "03073" -> f Gallery
  "03074" -> f Courtyard
  "03075" -> f DiningRoom
  -- Echoes of the Past
  "03127" -> f EntryHall
  "03128" -> f HistoricalSocietyMeetingRoom
  "03129" -> f HistoricalSocietyRecordOffice_129
  "03130" -> f HistoricalSocietyHistoricalMuseum_130
  "03131" -> f QuietHalls_131
  "03132" -> f HistoricalSocietyHistoricalMuseum_132
  "03133" -> f HistoricalSocietyHistoricalLibrary_133
  "03134" -> f HistoricalSocietyReadingRoom
  "03135" -> f QuietHalls_135
  "03136" -> f HistoricalSocietyHistoricalLibrary_136
  "03137" -> f HistoricalSocietyPeabodysOffice
  "03138" -> f HistoricalSocietyRecordOffice_138
  "03139" -> f HiddenLibrary
  -- The Unspeakable Oath
  "03168" -> f AsylumHallsWesternPatientWing_168
  "03169" -> f AsylumHallsWesternPatientWing_169
  "03170" -> f AsylumHallsEasternPatientWing_170
  "03171" -> f AsylumHallsEasternPatientWing_171
  "03172" -> f Kitchen
  "03173" -> f MessHall
  "03174" -> f Infirmary
  "03175" -> f Yard
  "03176" -> f Garden
  "03177" -> f BasementHall
  "03178" -> f PatientConfinementDanielsCell
  "03179" -> f PatientConfinementOccupiedCell
  "03180" -> f PatientConfinementDrearyCell
  "03181" -> f PatientConfinementFamiliarCell
  -- A Phantom of Truth
  "03208" -> f Montparnasse
  "03209" -> f Montmartre209
  "03210" -> f Montmartre210
  "03211" -> f GrandGuignol
  "03212" -> f OperaGarnier212
  "03213" -> f OperaGarnier213
  "03214" -> f GareDOrsay
  "03215" -> f PereLachaiseCemetery
  "03216" -> f CanalSaintMartin
  "03217" -> f LeMarais217
  "03218" -> f LeMarais218
  "03219" -> f NotreDame
  "03220" -> f GardensOfLuxembourg
  -- The Pallid Mask
  "03247" -> f TheGateToHell
  "03248" -> f StoneArchways
  "03249" -> f CryptOfTheSepulchralLamp
  "03250" -> f BoneFilledCaverns
  "03251" -> f WellOfSouls
  "03252" -> f CandlelitTunnels
  "03253" -> f LabyrinthOfBones
  "03254" -> f NarrowShaft
  "03255" -> f ShiveringPools
  "03256" -> f BlockedPassage
  "03257" -> f TombOfShadows
  -- Black Stars Rise
  "03283" -> f PorteDeLAvancee
  "03284" -> f GrandRue
  "03285" -> f OuterWall_285
  "03286" -> f OuterWall_286
  "03287" -> f NorthTower_287
  "03288" -> f NorthTower_288
  "03289" -> f BrokenSteps_289
  "03290" -> f BrokenSteps_290
  "03291" -> f AbbeyChurch
  "03292" -> f ChoeurGothique_292
  "03293" -> f ChoeurGothique_293
  "03294" -> f Cloister
  "03295" -> f KnightsHall
  "03296" -> f ChapelOfStAubertThePathIsOpen
  "03297" -> f ChapelOfStAubertWatersForbidden
  "03298" -> f AbbeyTowerThePathIsOpen
  "03299" -> f AbbeyTowerSpiresForbidden
  -- Dim Carcosa
  "03325b" -> f ShoresOfHali
  "03326b" -> f BleakPlainsStarsOfAldebaran
  "03326d" -> f BleakPlainsBleakDesolation
  "03327b" -> f RuinsOfCarcosaInhabitantOfCarcosa
  "03327d" -> f RuinsOfCarcosaAMomentsRest
  "03327f" -> f RuinsOfCarcosaTheCoffin
  "03328b" -> f DimStreetsMappingTheStreets
  "03328d" -> f DimStreetsTheKingsParade
  "03328f" -> f DimStreetsTheArchway
  "03329b" -> f DepthsOfDemheTheHeightOfTheDepths
  "03329d" -> f DepthsOfDemheStepsOfThePalace
  "03330b" -> f DarkSpires
  "03331b" -> f PalaceOfTheKing
  -- The Forgotten Age
  -- The Untamed Wilds
  "04050" -> f ExpeditionCamp
  -- Rainforest
  "04069" -> f PathOfThorns
  "04070" -> f RiverCanyon
  "04071" -> f RopeBridge
  "04072" -> f SerpentsHaven
  "04073" -> f CircuitousTrail
  "04074" -> f TempleOfTheFang
  "04075" -> f OvergrownRuins
  -- Return to Night of the Zealot
  -- Return to the Gathering
  "50013" -> f StudyAberrantGateway
  "50014" -> f GuestHall
  "50015" -> f Bedroom
  "50016" -> f Bathroom
  "50017" -> f HoleInTheWall
  "50018" -> f ReturnToAttic
  "50019" -> f FarAboveYourHouse
  "50020" -> f ReturnToCellar
  "50021" -> f DeepBelowYourHouse
  -- Return to the Midnight Masks
  "50027" -> f EasttownArkhamPoliceStation
  "50028" -> f NorthsideTrainStation
  "50029" -> f MiskatonicUniversityMiskatonicMuseum
  "50030" -> f RivertownAbandonedWarehouse
  -- Return to the Devourer Below
  "50033" -> f ArkhamWoodsGreatWillow
  "50034" -> f ArkhamWoodsLakeside
  "50035" -> f ArkhamWoodsCorpseRiddenClearing
  "50036" -> f ArkhamWoodsWoodenBridge
  -- The Curse of the Rougarou
  "81007" -> f CursedShores
  "81008" -> f GardenDistrict
  "81009" -> f Broadmoor
  "81010" -> f BrackishWaters
  "81011" -> f AudubonPark
  "81012" -> f FauborgMarigny
  "81013" -> f ForgottenMarsh
  "81014" -> f TrappersCabin
  "81015" -> f TwistedUnderbrush
  "81016" -> f FoulSwamp
  "81017" -> f RitualGrounds
  "81018" -> f OvergrownCairns
  -- Carnevale of Horrors
  "82006b" -> f Gondola
  "82008" -> f SanMarcoBasilica
  "82009" -> f CanalSide
  "82010" -> f StreetsOfVenice
  "82011" -> f RialtoBridge
  "82012" -> f VenetianGarden
  "82013" -> f BridgeOfSighs
  "82014" -> f FloodedSquare
  "82015" -> f AccademiaBridge
  "82016" -> f TheGuardian
  _ -> error "unhandled"

allLocations :: HashMap CardCode (LocationId -> Location)
allLocations = mapFromList $ map
  (cbCardCode &&& cbCardBuilder)
  [ -- Night of the Zealot
    -- The Gathering
    Location <$> study
  , Location <$> hallway
  , Location <$> attic
  , Location <$> cellar
  , Location <$> parlor
  , Location <$> yourHouse
  -- The Midnight Masks
  , Location <$> rivertown
  , Location <$> southsideHistoricalSociety
  , Location <$> southsideMasBoardingHouse
  , Location <$> stMarysHospital
  , Location <$> miskatonicUniversity
  , Location <$> downtownFirstBankOfArkham
  , Location <$> downtownArkhamAsylum
  , Location <$> easttown
  , Location <$> graveyard
  , Location <$> northside
  , Location <$> mainPath
  -- The Devourer Below
  , Location <$> arkhamWoodsUnhallowedGround
  , Location <$> arkhamWoodsTwistingPaths
  , Location <$> arkhamWoodsOldHouse
  , Location <$> arkhamWoodsCliffside
  , Location <$> arkhamWoodsTangledThicket
  , Location <$> arkhamWoodsQuietGlade
  , Location <$> ritualSite
  -- The Dunwich Legacy
  -- Extracurricular Activity
  , Location <$> miskatonicQuad
  , Location <$> humanitiesBuilding
  , Location <$> orneLibrary
  , Location <$> studentUnion
  , Location <$> dormitories
  , Location <$> administrationBuilding
  , Location <$> facultyOfficesTheNightIsStillYoung
  , Location <$> facultyOfficesTheHourIsLate
  , Location <$> scienceBuilding
  , Location <$> alchemyLabs
  -- The House Always Wins
  , Location <$> laBellaLuna
  , Location <$> cloverClubLounge
  , Location <$> cloverClubBar
  , Location <$> cloverClubCardroom
  , Location <$> darkenedHall
  , Location <$> artGallery
  , Location <$> vipArea
  , Location <$> backAlley
  -- The Miskatonic Museum
  , Location <$> museumEntrance
  , Location <$> museumHalls
  , Location <$> securityOffice_128
  , Location <$> securityOffice_129
  , Location <$> administrationOffice_130
  , Location <$> administrationOffice_131
  , Location <$> exhibitHallAthabaskanExhibit
  , Location <$> exhibitHallMedusaExhibit
  , Location <$> exhibitHallNatureExhibit
  , Location <$> exhibitHallEgyptianExhibit
  , Location <$> exhibitHallHallOfTheDead
  , Location <$> exhibitHallRestrictedHall
  -- The Essex County Express
  , Location <$> passengerCar_167
  , Location <$> passengerCar_168
  , Location <$> passengerCar_169
  , Location <$> passengerCar_170
  , Location <$> passengerCar_171
  , Location <$> sleepingCar
  , Location <$> diningCar
  , Location <$> parlorCar
  , Location <$> engineCar_175
  , Location <$> engineCar_176
  , Location <$> engineCar_177
  -- Blood on the Altar
  , Location <$> villageCommons
  , Location <$> bishopsBrook_202
  , Location <$> bishopsBrook_203
  , Location <$> burnedRuins_204
  , Location <$> burnedRuins_205
  , Location <$> osbornsGeneralStore_206
  , Location <$> osbornsGeneralStore_207
  , Location <$> congregationalChurch_208
  , Location <$> congregationalChurch_209
  , Location <$> houseInTheReeds_210
  , Location <$> houseInTheReeds_211
  , Location <$> schoolhouse_212
  , Location <$> schoolhouse_213
  , Location <$> theHiddenChamber
  -- Undimensioned and Unseen
  , Location <$> dunwichVillage_242
  , Location <$> dunwichVillage_243
  , Location <$> coldSpringGlen_244
  , Location <$> coldSpringGlen_245
  , Location <$> tenAcreMeadow_246
  , Location <$> tenAcreMeadow_247
  , Location <$> blastedHeath_248
  , Location <$> blastedHeath_249
  , Location <$> whateleyRuins_250
  , Location <$> whateleyRuins_251
  , Location <$> devilsHopYard_252
  , Location <$> devilsHopYard_253
  -- Where Doom Awaits
  , Location <$> baseOfTheHill
  , Location <$> ascendingPath
  , Location <$> sentinelPeak
  , Location <$> slaughteredWoods
  , Location <$> eerieGlade
  , Location <$> destroyedPath
  , Location <$> frozenSpring
  , Location <$> dimensionalGap
  , Location <$> aTearInThePath
  , Location <$> uprootedWoods
  , Location <$> lostMemories
  -- Lost in Time and Space
  , Location <$> anotherDimension
  , Location <$> theEdgeOfTheUniverse
  , Location <$> tearThroughTime
  , Location <$> tearThroughSpace
  , Location <$> prismaticCascade
  , Location <$> endlessBridge
  , Location <$> stepsOfYhagharl
  , Location <$> dimensionalDoorway
  -- The Path to Carcosa
  -- Curtain Call
  , Location <$> theatre
  , Location <$> lobby
  , Location <$> balcony
  , Location <$> backstage
  , Location <$> lightingBox
  , Location <$> boxOffice
  , Location <$> greenRoom
  , Location <$> dressingRoom
  , Location <$> rehearsalRoom
  , Location <$> trapRoom
  -- The Last King
  , Location <$> foyer
  , Location <$> ballroom
  , Location <$> livingRoom
  , Location <$> gallery
  , Location <$> courtyard
  , Location <$> diningRoom
  -- Echoes of the Past
  , Location <$> entryHall
  , Location <$> historicalSocietyMeetingRoom
  , Location <$> historicalSocietyRecordOffice_129
  , Location <$> historicalSocietyHistoricalMuseum_130
  , Location <$> quietHalls_131
  , Location <$> historicalSocietyHistoricalMuseum_132
  , Location <$> historicalSocietyHistoricalLibrary_133
  , Location <$> historicalSocietyReadingRoom
  , Location <$> quietHalls_135
  , Location <$> historicalSocietyHistoricalLibrary_136
  , Location <$> historicalSocietyPeabodysOffice
  , Location <$> historicalSocietyRecordOffice_138
  , Location <$> hiddenLibrary
  -- The Unspeakable Oath
  , Location <$> asylumHallsWesternPatientWing_168
  , Location <$> asylumHallsWesternPatientWing_169
  , Location <$> asylumHallsEasternPatientWing_170
  , Location <$> asylumHallsEasternPatientWing_171
  , Location <$> kitchen
  , Location <$> messHall
  , Location <$> infirmary
  , Location <$> yard
  , Location <$> garden
  , Location <$> basementHall
  , Location <$> patientConfinementDanielsCell
  , Location <$> patientConfinementOccupiedCell
  , Location <$> patientConfinementDrearyCell
  , Location <$> patientConfinementFamiliarCell
  -- A Phantom of Truth
  , Location <$> montparnasse
  , Location <$> montmartre209
  , Location <$> montmartre210
  , Location <$> grandGuignol
  , Location <$> operaGarnier212
  , Location <$> operaGarnier213
  , Location <$> gareDOrsay
  , Location <$> pereLachaiseCemetery
  , Location <$> canalSaintMartin
  , Location <$> leMarais217
  , Location <$> leMarais218
  , Location <$> notreDame
  , Location <$> gardensOfLuxembourg
  -- The Pallid Mask
  , Location <$> theGateToHell
  , Location <$> stoneArchways
  , Location <$> cryptOfTheSepulchralLamp
  , Location <$> boneFilledCaverns
  , Location <$> wellOfSouls
  , Location <$> candlelitTunnels
  , Location <$> labyrinthOfBones
  , Location <$> narrowShaft
  , Location <$> shiveringPools
  , Location <$> blockedPassage
  , Location <$> tombOfShadows
  -- Black Stars Rise
  , Location <$> porteDeLAvancee
  , Location <$> grandRue
  , Location <$> outerWall_285
  , Location <$> outerWall_286
  , Location <$> northTower_287
  , Location <$> northTower_288
  , Location <$> brokenSteps_289
  , Location <$> brokenSteps_290
  , Location <$> abbeyChurch
  , Location <$> choeurGothique_292
  , Location <$> choeurGothique_293
  , Location <$> cloister
  , Location <$> knightsHall
  , Location <$> chapelOfStAubertThePathIsOpen
  , Location <$> chapelOfStAubertWatersForbidden
  , Location <$> abbeyTowerThePathIsOpen
  , Location <$> abbeyTowerSpiresForbidden
  -- Dim Carcosa
  ,  Location <$> shoresOfHali
  ,  Location <$> bleakPlainsStarsOfAldebaran
  ,  Location <$> bleakPlainsBleakDesolation
  ,  Location <$> ruinsOfCarcosaInhabitantOfCarcosa
  ,  Location <$> ruinsOfCarcosaAMomentsRest
  ,  Location <$> ruinsOfCarcosaTheCoffin
  ,  Location <$> dimStreetsMappingTheStreets
  ,  Location <$> dimStreetsTheKingsParade
  ,  Location <$> dimStreetsTheArchway
  ,  Location <$> depthsOfDemheTheHeightOfTheDepths
  ,  Location <$> depthsOfDemheStepsOfThePalace
  ,  Location <$> darkSpires
  ,  Location <$> palaceOfTheKing
  -- The Forgotten Age
  -- The Untamed Wilds
  ,  Location <$> expeditionCamp
  -- Return to Night of the Zealot
  -- Return to the Gathering
  , Location <$> studyAberrantGateway
  , Location <$> guestHall
  , Location <$> bedroom
  , Location <$> bathroom
  , Location <$> holeInTheWall
  , Location <$> returnToAttic
  , Location <$> farAboveYourHouse
  , Location <$> returnToCellar
  , Location <$> deepBelowYourHouse
  -- Return to the Midnight Masks
  , Location <$> easttownArkhamPoliceStation
  , Location <$> northsideTrainStation
  , Location <$> miskatonicUniversityMiskatonicMuseum
  , Location <$> rivertownAbandonedWarehouse
  -- Return to the Devourer Below
  , Location <$> arkhamWoodsGreatWillow
  , Location <$> arkhamWoodsLakeside
  , Location <$> arkhamWoodsCorpseRiddenClearing
  , Location <$> arkhamWoodsWoodenBridge
  -- The Curse of the Rougarou
  , Location <$> cursedShores
  , Location <$> gardenDistrict
  , Location <$> broadmoor
  , Location <$> brackishWaters
  , Location <$> audubonPark
  , Location <$> fauborgMarigny
  , Location <$> forgottenMarsh
  , Location <$> trappersCabin
  , Location <$> twistedUnderbrush
  , Location <$> foulSwamp
  , Location <$> ritualGrounds
  , Location <$> overgrownCairns
  -- Carnevale of Horrors
  , Location <$> gondola
  , Location <$> sanMarcoBasilica
  , Location <$> canalSide
  , Location <$> streetsOfVenice
  , Location <$> rialtoBridge
  , Location <$> venetianGarden
  , Location <$> bridgeOfSighs
  , Location <$> floodedSquare
  , Location <$> accademiaBridge
  , Location <$> theGuardian
  ]
