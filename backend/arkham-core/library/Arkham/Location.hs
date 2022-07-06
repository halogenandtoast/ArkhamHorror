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
import Arkham.Location.Locations
import Arkham.Location.Runner
import Arkham.Message
import Arkham.Name
import Data.UUID (nil)
import Data.Typeable

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
    case cCode of
      -- Night of the Zealot
      -- The Gathering
      "01111" -> Location . Study <$> parseJSON v
      "01112" -> Location . Hallway <$> parseJSON v
      "01113" -> Location . Attic <$> parseJSON v
      "01114" -> Location . Cellar <$> parseJSON v
      "01115" -> Location . Parlor <$> parseJSON v
      -- The Midnight Masks
      "01124" -> Location . YourHouse <$> parseJSON v
      "01125" -> Location . Rivertown <$> parseJSON v
      "01126" -> Location . SouthsideHistoricalSociety <$> parseJSON v
      "01127" -> Location . SouthsideMasBoardingHouse <$> parseJSON v
      "01128" -> Location . StMarysHospital <$> parseJSON v
      "01129" -> Location . MiskatonicUniversity <$> parseJSON v
      "01130" -> Location . DowntownFirstBankOfArkham <$> parseJSON v
      "01131" -> Location . DowntownArkhamAsylum <$> parseJSON v
      "01132" -> Location . Easttown <$> parseJSON v
      "01133" -> Location . Graveyard <$> parseJSON v
      "01134" -> Location . Northside <$> parseJSON v
      -- The Devourer Below
      "01149" -> Location . MainPath <$> parseJSON v
      "01150" -> Location . ArkhamWoodsUnhallowedGround <$> parseJSON v
      "01151" -> Location . ArkhamWoodsTwistingPaths <$> parseJSON v
      "01152" -> Location . ArkhamWoodsOldHouse <$> parseJSON v
      "01153" -> Location . ArkhamWoodsCliffside <$> parseJSON v
      "01154" -> Location . ArkhamWoodsTangledThicket <$> parseJSON v
      "01155" -> Location . ArkhamWoodsQuietGlade <$> parseJSON v
      "01156" -> Location . RitualSite <$> parseJSON v
      -- The Dunwich Legacy
      -- Extracurricular Activity
      "02048" -> Location . MiskatonicQuad <$> parseJSON v
      "02049" -> Location . HumanitiesBuilding <$> parseJSON v
      "02050" -> Location . OrneLibrary <$> parseJSON v
      "02051" -> Location . StudentUnion <$> parseJSON v
      "02052" -> Location . Dormitories <$> parseJSON v
      "02053" -> Location . AdministrationBuilding <$> parseJSON v
      "02054" -> Location . FacultyOfficesTheNightIsStillYoung <$> parseJSON v
      "02055" -> Location . FacultyOfficesTheHourIsLate <$> parseJSON v
      "02056" -> Location . ScienceBuilding <$> parseJSON v
      "02057" -> Location . AlchemyLabs <$> parseJSON v
      -- The House Always Wins
      "02070" -> Location . LaBellaLuna <$> parseJSON v
      "02071" -> Location . CloverClubLounge <$> parseJSON v
      "02072" -> Location . CloverClubBar <$> parseJSON v
      "02073" -> Location . CloverClubCardroom <$> parseJSON v
      "02074" -> Location . DarkenedHall <$> parseJSON v
      "02075" -> Location . ArtGallery <$> parseJSON v
      "02076" -> Location . VipArea <$> parseJSON v
      "02077" -> Location . BackAlley <$> parseJSON v
      -- The Miskatonic Museum
      "02126" -> Location . MuseumEntrance <$> parseJSON v
      "02127" -> Location . MuseumHalls <$> parseJSON v
      "02128" -> Location . SecurityOffice_128 <$> parseJSON v
      "02129" -> Location . SecurityOffice_129 <$> parseJSON v
      "02130" -> Location . AdministrationOffice_130 <$> parseJSON v
      "02131" -> Location . AdministrationOffice_131 <$> parseJSON v
      "02132" -> Location . ExhibitHallAthabaskanExhibit <$> parseJSON v
      "02133" -> Location . ExhibitHallMedusaExhibit <$> parseJSON v
      "02134" -> Location . ExhibitHallNatureExhibit <$> parseJSON v
      "02135" -> Location . ExhibitHallEgyptianExhibit <$> parseJSON v
      "02136" -> Location . ExhibitHallHallOfTheDead <$> parseJSON v
      "02137" -> Location . ExhibitHallRestrictedHall <$> parseJSON v
      -- The Essex County Express
      "02167" -> Location . PassengerCar_167 <$> parseJSON v
      "02168" -> Location . PassengerCar_168 <$> parseJSON v
      "02169" -> Location . PassengerCar_169 <$> parseJSON v
      "02170" -> Location . PassengerCar_170 <$> parseJSON v
      "02171" -> Location . PassengerCar_171 <$> parseJSON v
      "02172" -> Location . SleepingCar <$> parseJSON v
      "02173" -> Location . DiningCar <$> parseJSON v
      "02174" -> Location . ParlorCar <$> parseJSON v
      "02175" -> Location . EngineCar_175 <$> parseJSON v
      "02176" -> Location . EngineCar_176 <$> parseJSON v
      "02177" -> Location . EngineCar_177 <$> parseJSON v
      -- Blood on the Altar
      "02201" -> Location . VillageCommons <$> parseJSON v
      "02202" -> Location . BishopsBrook_202 <$> parseJSON v
      "02203" -> Location . BishopsBrook_203 <$> parseJSON v
      "02204" -> Location . BurnedRuins_204 <$> parseJSON v
      "02205" -> Location . BurnedRuins_205 <$> parseJSON v
      "02206" -> Location . OsbornsGeneralStore_206 <$> parseJSON v
      "02207" -> Location . OsbornsGeneralStore_207 <$> parseJSON v
      "02208" -> Location . CongregationalChurch_208 <$> parseJSON v
      "02209" -> Location . CongregationalChurch_209 <$> parseJSON v
      "02210" -> Location . HouseInTheReeds_210 <$> parseJSON v
      "02211" -> Location . HouseInTheReeds_211 <$> parseJSON v
      "02212" -> Location . Schoolhouse_212 <$> parseJSON v
      "02213" -> Location . Schoolhouse_213 <$> parseJSON v
      "02214" -> Location . TheHiddenChamber <$> parseJSON v
      -- Undimensioned and Unseen
      "02242" -> Location . DunwichVillage_242 <$> parseJSON v
      "02243" -> Location . DunwichVillage_243 <$> parseJSON v
      "02244" -> Location . ColdSpringGlen_244 <$> parseJSON v
      "02245" -> Location . ColdSpringGlen_245 <$> parseJSON v
      "02246" -> Location . TenAcreMeadow_246 <$> parseJSON v
      "02247" -> Location . TenAcreMeadow_247 <$> parseJSON v
      "02248" -> Location . BlastedHeath_248 <$> parseJSON v
      "02249" -> Location . BlastedHeath_249 <$> parseJSON v
      "02250" -> Location . WhateleyRuins_250 <$> parseJSON v
      "02251" -> Location . WhateleyRuins_251 <$> parseJSON v
      "02252" -> Location . DevilsHopYard_252 <$> parseJSON v
      "02253" -> Location . DevilsHopYard_253 <$> parseJSON v
      -- Where Doom Awaits
      "02282" -> Location . BaseOfTheHill <$> parseJSON v
      "02283" -> Location . AscendingPath <$> parseJSON v
      "02284" -> Location . SentinelPeak <$> parseJSON v
      "02285" -> Location . SlaughteredWoods <$> parseJSON v
      "02286" -> Location . EerieGlade <$> parseJSON v
      "02287" -> Location . DestroyedPath <$> parseJSON v
      "02288" -> Location . FrozenSpring <$> parseJSON v
      "02289" -> Location . DimensionalGap <$> parseJSON v
      "02290" -> Location . ATearInThePath <$> parseJSON v
      "02291" -> Location . UprootedWoods <$> parseJSON v
      "02292" -> Location . LostMemories <$> parseJSON v
      -- Lost in Time and Space
      "02310" -> Location . AnotherDimension <$> parseJSON v
      "02311" -> Location . TheEdgeOfTheUniverse <$> parseJSON v
      "02312" -> Location . TearThroughTime <$> parseJSON v
      "02313" -> Location . TearThroughSpace <$> parseJSON v
      "02314" -> Location . PrismaticCascade <$> parseJSON v
      "02315" -> Location . EndlessBridge <$> parseJSON v
      "02316" -> Location . StepsOfYhagharl <$> parseJSON v
      "02317" -> Location . DimensionalDoorway <$> parseJSON v
      -- The Path To Carcosa
      -- Curtain Call
      "03049" -> Location . Theatre <$> parseJSON v
      "03050" -> Location . Lobby <$> parseJSON v
      "03051" -> Location . Balcony <$> parseJSON v
      "03052" -> Location . Backstage <$> parseJSON v
      "03053" -> Location . LightingBox <$> parseJSON v
      "03054" -> Location . BoxOffice <$> parseJSON v
      "03055" -> Location . GreenRoom <$> parseJSON v
      "03056" -> Location . DressingRoom <$> parseJSON v
      "03057" -> Location . RehearsalRoom <$> parseJSON v
      "03058" -> Location . TrapRoom <$> parseJSON v
      -- The Last King
      "03070" -> Location . Foyer <$> parseJSON v
      "03071" -> Location . Ballroom <$> parseJSON v
      "03072" -> Location . LivingRoom <$> parseJSON v
      "03073" -> Location . Gallery <$> parseJSON v
      "03074" -> Location . Courtyard <$> parseJSON v
      "03075" -> Location . DiningRoom <$> parseJSON v
      -- Echoes of the Past
      "03127" -> Location . EntryHall <$> parseJSON v
      "03128" -> Location . HistoricalSocietyMeetingRoom <$> parseJSON v
      "03129" -> Location . HistoricalSocietyRecordOffice_129 <$> parseJSON v
      "03130" -> Location . HistoricalSocietyHistoricalMuseum_130 <$> parseJSON v
      "03131" -> Location . QuietHalls_131 <$> parseJSON v
      "03132" -> Location . HistoricalSocietyHistoricalMuseum_132 <$> parseJSON v
      "03133" -> Location . HistoricalSocietyHistoricalLibrary_133 <$> parseJSON v
      "03134" -> Location . HistoricalSocietyReadingRoom <$> parseJSON v
      "03135" -> Location . QuietHalls_135 <$> parseJSON v
      "03136" -> Location . HistoricalSocietyHistoricalLibrary_136 <$> parseJSON v
      "03137" -> Location . HistoricalSocietyPeabodysOffice <$> parseJSON v
      "03138" -> Location . HistoricalSocietyRecordOffice_138 <$> parseJSON v
      "03139" -> Location . HiddenLibrary <$> parseJSON v
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
  ]
