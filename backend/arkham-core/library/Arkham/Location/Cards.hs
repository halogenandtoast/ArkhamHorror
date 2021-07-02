module Arkham.Location.Cards where

import Arkham.Prelude

import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.CardType
import Arkham.Types.EncounterSet hiding (Dunwich)
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Name
import Arkham.Types.Trait

location :: CardCode -> Name -> EncounterSet -> CardDef
location cardCode name encounterSet = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = LocationType
  , cdWeakness = False
  , cdClassSymbol = Nothing
  , cdSkills = mempty
  , cdCardTraits = mempty
  , cdKeywords = mempty
  , cdFast = False
  , cdWindows = mempty
  , cdAction = Nothing
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdCommitRestrictions = mempty
  , cdAttackOfOpportunityModifiers = mempty
  , cdPermanent = False
  , cdEncounterSet = Just encounterSet
  , cdUnique = False
  }

allLocationCards :: HashMap CardCode CardDef
allLocationCards = mapFromList
  [ ("01111", study)
  , ("01112", hallway)
  , ("01113", attic)
  , ("01114", cellar)
  , ("01115", parlor)
  , ("01124", yourHouse)
  , ("01125", rivertown)
  , ("01126", southsideHistoricalSociety)
  , ("01127", southsideMasBoardingHouse)
  , ("01128", stMarysHospital)
  , ("01129", miskatonicUniversity)
  , ("01130", downtownFirstBankOfArkham)
  , ("01131", downtownArkhamAsylum)
  , ("01132", easttown)
  , ("01133", graveyard)
  , ("01134", northside)
  , ("01149", mainPath)
  , ("01150", arkhamWoodsUnhallowedGround)
  , ("01151", arkhamWoodsTwistingPaths)
  , ("01152", arkhamWoodsOldHouse)
  , ("01153", arkhamWoodsCliffside)
  , ("01154", arkhamWoodsTangledThicket)
  , ("01155", arkhamWoodsQuietGlade)
  , ("01156", ritualSite)
  , ("02048", miskatonicQuad)
  , ("02049", humanitiesBuilding)
  , ("02050", orneLibrary)
  , ("02051", studentUnion)
  , ("02052", dormitories)
  , ("02053", administrationBuilding)
  , ("02054", facultyOfficesTheNightIsStillYoung)
  , ("02055", facultyOfficesTheHourIsLate)
  , ("02056", scienceBuilding)
  , ("02057", alchemyLabs)
  , ("02070", laBellaLuna)
  , ("02071", cloverClubLounge)
  , ("02072", cloverClubBar)
  , ("02073", cloverClubCardroom)
  , ("02074", darkenedHall)
  , ("02075", artGallery)
  , ("02076", vipArea)
  , ("02077", backAlley)
  , ("02126", museumEntrance)
  , ("02127", museumHalls)
  , ("02128", securityOffice_128)
  , ("02129", securityOffice_129)
  , ("02130", administrationOffice_130)
  , ("02131", administrationOffice_131)
  , ("02132", exhibitHallAthabaskanExhibit)
  , ("02133", exhibitHallMedusaExhibit)
  , ("02134", exhibitHallNatureExhibit)
  , ("02135", exhibitHallEgyptianExhibit)
  , ("02136", exhibitHallHallOfTheDead)
  , ("02137", exhibitHallRestrictedHall)
  , ("02167", passengerCar_167)
  , ("02168", passengerCar_168)
  , ("02169", passengerCar_169)
  , ("02170", passengerCar_170)
  , ("02171", passengerCar_171)
  , ("02172", sleepingCar)
  , ("02173", diningCar)
  , ("02174", parlorCar)
  , ("02175", engineCar_175)
  , ("02176", engineCar_176)
  , ("02177", engineCar_177)
  , ("02201", villageCommons)
  , ("02202", bishopsBrook_202)
  , ("02203", bishopsBrook_203)
  , ("02204", burnedRuins_204)
  , ("02205", burnedRuins_205)
  , ("02206", osbornsGeneralStore_206)
  , ("02207", osbornsGeneralStore_207)
  , ("02208", congregationalChurch_208)
  , ("02209", congregationalChurch_209)
  , ("02210", houseInTheReeds_210)
  , ("02211", houseInTheReeds_211)
  , ("02212", schoolhouse_212)
  , ("02213", schoolhouse_213)
  , ("02214", theHiddenChamber)
  , ("02242", dunwichVillage_242)
  , ("02243", dunwichVillage_243)
  , ("02244", coldSpringGlen_244)
  , ("02245", coldSpringGlen_245)
  , ("02246", tenAcreMeadow_246)
  , ("02247", tenAcreMeadow_247)
  , ("02248", blastedHeath_248)
  , ("02249", blastedHeath_249)
  , ("02250", whateleyRuins_250)
  , ("02251", whateleyRuins_251)
  , ("02252", devilsHopYard_252)
  , ("02253", devilsHopYard_253)
  , ("02282", baseOfTheHill)
  , ("02283", ascendingPath)
  , ("02284", sentinelPeak)
  , ("02285", slaugteredWoods)
  , ("02286", eerieGlade)
  , ("02287", destroyedPath)
  , ("02288", frozenSpring)
  , ("02289", dimensionalGap)
  , ("02290", aTearInThePath)
  , ("02291", uprootedWoods)
  , ("02292", lostMemories)
  , ("02320", anotherDimension)
  , ("02321", theEdgeOfTheUniverse)
  , ("02322", tearThroughTime)
  , ("02324", tearThroughSpace)
  , ("02325", prismaticCascade)
  , ("02326", endlessBridge)
  , ("02327", stepsOfYhagharl)
  , ("02328", dimensionalDoorway)
  , ("50013", studyAberrantGateway)
  , ("50014", guestHall)
  , ("50015", bedroom)
  , ("50016", bathroom)
  , ("50017", holeInTheWall)
  , ("50018", returnToAttic)
  , ("50019", farAboveYourHouse)
  , ("50020", returnToCellar)
  , ("50021", deepBelowYourHouse)
  , ("50027", easttownArkhamPoliceStation)
  , ("50028", northsideTrainStation)
  , ("50029", miskatonicUniversityMiskatonicMuseum)
  , ("50030", rivertownAbandonedWarehouse)
  , ("50033", arkhamWoodsGreatWillow)
  , ("50034", arkhamWoodsLakeside)
  , ("50035", arkhamWoodsCorpseRiddenClearing)
  , ("50036", arkhamWoodsWoodenBridge)
  , ("81007", cursedShores)
  , ("81008", gardenDistrict)
  , ("81009", broadmoor)
  , ("81010", brackishWaters)
  , ("81011", audubonPark)
  , ("81012", fauborgMarigny)
  , ("81013", forgottenMarsh)
  , ("81014", trappersCabin)
  , ("81015", twistedUnderbrush)
  , ("81016", foulSwamp)
  , ("81017", ritualGrounds)
  , ("81018", overgrownCairns)
  ]

study :: CardDef
study = location "01111" "Study" TheGathering

hallway :: CardDef
hallway = location "01112" "Hallway" TheGathering

attic :: CardDef
attic =
  (location "01113" "Attic" TheGathering)
    { cdVictoryPoints = Just 1
    }

cellar :: CardDef
cellar =
  (location "01114" "Cellar" TheGathering)
    { cdVictoryPoints = Just 1
    }

parlor :: CardDef
parlor = location "01115" "Parlor" TheGathering

yourHouse :: CardDef
yourHouse =
  (location "01124" "Your House" TheMidnightMasks)
    { cdCardTraits = singleton Arkham
    }

rivertown :: CardDef
rivertown =
  (location "01125" "Rivertown" TheMidnightMasks)
    { cdCardTraits = setFromList [Arkham, Central]
    }

southsideHistoricalSociety :: CardDef
southsideHistoricalSociety =
  (location "01126" ("Southside" <:> "Historical Society") TheMidnightMasks)
    { cdCardTraits = singleton Arkham
    }

southsideMasBoardingHouse :: CardDef
southsideMasBoardingHouse =
  (location "01127" ("Southside" <:> "Ma's Boarding House") TheMidnightMasks)
    { cdCardTraits = singleton Arkham
    }

stMarysHospital :: CardDef
stMarysHospital =
  (location "01128" "St. Mary's Hospital" TheMidnightMasks)
    { cdCardTraits = singleton Arkham
    }

miskatonicUniversity :: CardDef
miskatonicUniversity =
  (location "01129" "Miskatonic University" TheMidnightMasks)
    { cdCardTraits = singleton Arkham
    , cdVictoryPoints = Just 1
    }

downtownFirstBankOfArkham :: CardDef
downtownFirstBankOfArkham =
  (location "01130" ("Downtown" <:> "First Bank of Arkham") TheMidnightMasks)
    { cdCardTraits = singleton Arkham
    }

downtownArkhamAsylum :: CardDef
downtownArkhamAsylum =
  (location "01131" ("Downtown" <:> "Arkham Asylum") TheMidnightMasks)
    { cdCardTraits = singleton Arkham
    , cdVictoryPoints = Just 1
    }

easttown :: CardDef
easttown =
  (location "01132" "Easttown" TheMidnightMasks)
    { cdCardTraits = singleton Arkham
    }

graveyard :: CardDef
graveyard =
  (location "01133" "Graveyard" TheMidnightMasks)
    { cdCardTraits = singleton Arkham
    , cdVictoryPoints = Just 1
    }

northside :: CardDef
northside =
  (location "01134" "Northside" TheMidnightMasks)
    { cdCardTraits = singleton Arkham
    , cdVictoryPoints = Just 1
    }

mainPath :: CardDef
mainPath =
  (location "01149" "Main Path" TheDevourerBelow)
    { cdCardTraits = singleton Woods
    }

arkhamWoodsUnhallowedGround :: CardDef
arkhamWoodsUnhallowedGround =
  (location "01150" ("Arkham Woods" <:> "Unhallowed Ground") TheDevourerBelow)
    { cdCardTraits = singleton Woods
    }

arkhamWoodsTwistingPaths :: CardDef
arkhamWoodsTwistingPaths =
  (location "01151" ("Arkham Woods" <:> "Twisting Paths") TheDevourerBelow)
    { cdCardTraits = singleton Woods
    }

arkhamWoodsOldHouse :: CardDef
arkhamWoodsOldHouse =
  (location "01152" ("Arkham Woods" <:> "Old House") TheDevourerBelow)
    { cdCardTraits = singleton Woods
    }

arkhamWoodsCliffside :: CardDef
arkhamWoodsCliffside =
  (location "01153" ("Arkham Woods" <:> "Cliffside") TheDevourerBelow)
    { cdCardTraits = singleton Woods
    }

arkhamWoodsTangledThicket :: CardDef
arkhamWoodsTangledThicket =
  (location "01154" ("Arkham Woods" <:> "Tangled Thicket") TheDevourerBelow)
    { cdCardTraits = singleton Woods
    }

arkhamWoodsQuietGlade :: CardDef
arkhamWoodsQuietGlade =
  (location "01155" ("Arkham Woods" <:> "Quiet Glade") TheDevourerBelow)
    { cdCardTraits = singleton Woods
    }

ritualSite :: CardDef
ritualSite =
  (location "01156" "Ritual Site" TheDevourerBelow)
    { cdCardTraits = singleton Cave
    }

miskatonicQuad :: CardDef
miskatonicQuad =
  (location "02048" "Miskatonic Quad" ExtracurricularActivity)
    { cdCardTraits = singleton Miskatonic
    }

humanitiesBuilding :: CardDef
humanitiesBuilding =
  (location "02049" "Humanities Building" ExtracurricularActivity)
    { cdCardTraits = singleton Miskatonic
    }

orneLibrary :: CardDef
orneLibrary =
  (location "02050" "Orne Library" ExtracurricularActivity)
    { cdCardTraits = singleton Miskatonic
    , cdVictoryPoints = Just 1
    }

studentUnion :: CardDef
studentUnion =
  (location "02051" "Student Union" ExtracurricularActivity)
    { cdCardTraits = singleton Miskatonic
    }

dormitories :: CardDef
dormitories =
  (location "02052" "Dormitories" ExtracurricularActivity)
    { cdCardTraits = singleton Miskatonic
    , cdVictoryPoints = Just 1
    }

administrationBuilding :: CardDef
administrationBuilding =
  (location "02053" "Administration Building" ExtracurricularActivity)
    { cdCardTraits = singleton Miskatonic
    }

facultyOfficesTheNightIsStillYoung :: CardDef
facultyOfficesTheNightIsStillYoung =
  (location "02054" ("Faculty Offices" <:> "The Night is Still Young") ExtracurricularActivity)
    { cdCardTraits = singleton Miskatonic
    , cdVictoryPoints = Just 1
    }

facultyOfficesTheHourIsLate :: CardDef
facultyOfficesTheHourIsLate =
  (location "02055" ("Faculty Offices" <:> "The Hour is Late") ExtracurricularActivity)
    { cdCardTraits = singleton Miskatonic
    }

scienceBuilding :: CardDef
scienceBuilding =
  (location "02056" "Science Building" ExtracurricularActivity)
    { cdCardTraits = singleton Miskatonic
    }

alchemyLabs :: CardDef
alchemyLabs =
  (location "02057" "Alchemy Labs" ExtracurricularActivity)
    { cdCardTraits = singleton Miskatonic
    }

laBellaLuna :: CardDef
laBellaLuna =
  (location "02070" "La Bella Luna" TheHouseAlwaysWins)
    { cdCardTraits = singleton Arkham
    }

cloverClubLounge :: CardDef
cloverClubLounge =
  (location "02071" "Clover Club Lounge" TheHouseAlwaysWins)
    { cdCardTraits = singleton CloverClub
    }

cloverClubBar :: CardDef
cloverClubBar =
  (location "02072" "Clover Club Bar" TheHouseAlwaysWins)
    { cdCardTraits = singleton CloverClub
    }

cloverClubCardroom :: CardDef
cloverClubCardroom =
  (location "02073" "Clover Club Cardroom" TheHouseAlwaysWins)
    { cdCardTraits = singleton CloverClub
    }

darkenedHall :: CardDef
darkenedHall =
  (location "02074" "Darkened Hall" TheHouseAlwaysWins)
    { cdCardTraits = singleton CloverClub
    }

artGallery :: CardDef
artGallery =
  (location "02075" "Art Gallery" TheHouseAlwaysWins)
    { cdCardTraits = singleton CloverClub
    , cdVictoryPoints = Just 1
    }

vipArea :: CardDef
vipArea =
  (location "02076" "VIP Area" TheHouseAlwaysWins)
    { cdCardTraits = singleton CloverClub
    , cdVictoryPoints = Just 1
    }

backAlley :: CardDef
backAlley =
  (location "02077" "Back Alley" TheHouseAlwaysWins)
    { cdCardTraits = singleton CloverClub
    , cdVictoryPoints = Just 1
    }

museumEntrance :: CardDef
museumEntrance =
  (location "02126" "Museum Entrance" TheMiskatonicMuseum)
    { cdCardTraits = singleton Miskatonic
    }

museumHalls :: CardDef
museumHalls =
  (location "02127" "Museum Halls" TheMiskatonicMuseum)
    { cdCardTraits = singleton Miskatonic
    }

securityOffice_128 :: CardDef
securityOffice_128 =
  (location "02128" "Security Office" TheMiskatonicMuseum)
    { cdCardTraits = singleton Miskatonic
    }

securityOffice_129 :: CardDef
securityOffice_129 =
  (location "02129" "Security Office" TheMiskatonicMuseum)
    { cdCardTraits = singleton Miskatonic
    }

administrationOffice_130 :: CardDef
administrationOffice_130 =
  (location "02130" "Administration Office" TheMiskatonicMuseum)
    { cdCardTraits = singleton Miskatonic
    }

administrationOffice_131 :: CardDef
administrationOffice_131 =
  (location "02131" "Administration Office" TheMiskatonicMuseum)
    { cdCardTraits = singleton Miskatonic
    }

exhibitHallAthabaskanExhibit :: CardDef
exhibitHallAthabaskanExhibit =
  (location "02132" ("Exhibit Hall" <:> "Athabaskan Exhibit") TheMiskatonicMuseum)
    { cdCardTraits = setFromList [Miskatonic, Exhibit]
    }

exhibitHallMedusaExhibit :: CardDef
exhibitHallMedusaExhibit =
  (location "02133" ("Exhibit Hall" <:> "Medusa Exhibit") TheMiskatonicMuseum)
    { cdCardTraits = setFromList [Miskatonic, Exhibit]
    , cdVictoryPoints = Just 1
    }

exhibitHallNatureExhibit :: CardDef
exhibitHallNatureExhibit =
  (location "02134" ("Exhibit Hall" <:> "Nature Exhibit") TheMiskatonicMuseum)
    { cdCardTraits = setFromList [Miskatonic, Exhibit]
    , cdVictoryPoints = Just 1
    }

exhibitHallEgyptianExhibit :: CardDef
exhibitHallEgyptianExhibit =
  (location "02135" ("Exhibit Hall" <:> "Egyptian Exhibit") TheMiskatonicMuseum)
    { cdCardTraits = setFromList [Miskatonic, Exhibit]
    , cdVictoryPoints = Just 1
    }

exhibitHallHallOfTheDead :: CardDef
exhibitHallHallOfTheDead =
  (location "02136" ("Exhibit Hall" <:> "Hall of the Dead") TheMiskatonicMuseum)
    { cdCardTraits = setFromList [Miskatonic, Exhibit]
    , cdVictoryPoints = Just 1
    }

exhibitHallRestrictedHall :: CardDef
exhibitHallRestrictedHall =
  (location "02137" ("Exhibit Hall" <:> "Restricted Hall") TheMiskatonicMuseum)
    { cdCardTraits = setFromList [Miskatonic, Exhibit]
    , cdVictoryPoints = Just 1
    }

passengerCar_167 :: CardDef
passengerCar_167 =
  (location "02167" "Passenger Car" TheEssexCountyExpress)
    { cdCardTraits = singleton Train
    }

passengerCar_168 :: CardDef
passengerCar_168 =
  (location "02168" "Passenger Car" TheEssexCountyExpress)
    { cdCardTraits = singleton Train
    }

passengerCar_169 :: CardDef
passengerCar_169 =
  (location "02169" "Passenger Car" TheEssexCountyExpress)
    { cdCardTraits = singleton Train
    }

passengerCar_170 :: CardDef
passengerCar_170 =
  (location "02170" "Passenger Car" TheEssexCountyExpress)
    { cdCardTraits = singleton Train
    }

passengerCar_171 :: CardDef
passengerCar_171 =
  (location "02171" "Passenger Car" TheEssexCountyExpress)
    { cdCardTraits = singleton Train
    }

sleepingCar :: CardDef
sleepingCar =
  (location "02172" "Sleeping Car" TheEssexCountyExpress)
    { cdCardTraits = singleton Train
    }

diningCar :: CardDef
diningCar =
  (location "02173" "Dining Car" TheEssexCountyExpress)
    { cdCardTraits = singleton Train
    }

parlorCar :: CardDef
parlorCar =
  (location "02174" "Parlor Car" TheEssexCountyExpress)
    { cdCardTraits = singleton Train
    , cdVictoryPoints = Just 1
    }

engineCar_175 :: CardDef
engineCar_175 =
  (location "02175" "Engine Car" TheEssexCountyExpress)
    { cdCardTraits = singleton Train
    , cdVictoryPoints = Just 1
    }

engineCar_176 :: CardDef
engineCar_176 =
  (location "02176" "Engine Car" TheEssexCountyExpress)
    { cdCardTraits = singleton Train
    , cdVictoryPoints = Just 1
    }

engineCar_177 :: CardDef
engineCar_177 =
  (location "02177" "Engine Car" TheEssexCountyExpress)
    { cdCardTraits = singleton Train
    , cdVictoryPoints = Just 1
    }

villageCommons :: CardDef
villageCommons =
  (location "02201" "Village Commons" BloodOnTheAltar)
    { cdCardTraits = singleton Dunwich
    }

bishopsBrook_202 :: CardDef
bishopsBrook_202 =
  (location "02202" "Bishop's Brook" BloodOnTheAltar)
    { cdCardTraits = singleton Dunwich
    }

bishopsBrook_203 :: CardDef
bishopsBrook_203 =
  (location "02203" "Bishop's Brook" BloodOnTheAltar)
    { cdCardTraits = singleton Dunwich
    }

burnedRuins_204 :: CardDef
burnedRuins_204 =
  (location "02204" "Burned Ruins" BloodOnTheAltar)
    { cdCardTraits = singleton Dunwich
    }

burnedRuins_205 :: CardDef
burnedRuins_205 =
  (location "02205" "Burned Ruins" BloodOnTheAltar)
    { cdCardTraits = singleton Dunwich
    }

osbornsGeneralStore_206 :: CardDef
osbornsGeneralStore_206 =
  (location "02206" "Osborn's General Store" BloodOnTheAltar)
    { cdCardTraits = singleton Dunwich
    }

osbornsGeneralStore_207 :: CardDef
osbornsGeneralStore_207 =
  (location "02207" "Osborn's General Store" BloodOnTheAltar)
    { cdCardTraits = singleton Dunwich
    }

congregationalChurch_208 :: CardDef
congregationalChurch_208 =
  (location "02208" "Congregational Church" BloodOnTheAltar)
    { cdCardTraits = singleton Dunwich
    }

congregationalChurch_209 :: CardDef
congregationalChurch_209 =
  (location "02209" "Congregational Church" BloodOnTheAltar)
    { cdCardTraits = singleton Dunwich
    }

houseInTheReeds_210 :: CardDef
houseInTheReeds_210 =
  (location "02210" "House in the Reeds" BloodOnTheAltar)
    { cdCardTraits = singleton Dunwich
    }

houseInTheReeds_211 :: CardDef
houseInTheReeds_211 =
  (location "02211" "House in the Reeds" BloodOnTheAltar)
    { cdCardTraits = singleton Dunwich
    }

schoolhouse_212 :: CardDef
schoolhouse_212 =
  (location "02212" "Schoolhouse" BloodOnTheAltar)
    { cdCardTraits = singleton Dunwich
    }

schoolhouse_213 :: CardDef
schoolhouse_213 =
  (location "02213" "Schoolhouse" BloodOnTheAltar)
    { cdCardTraits = singleton Dunwich
    }

theHiddenChamber :: CardDef
theHiddenChamber =
  (location "02214" "The Hidden Chamber" BloodOnTheAltar)
    { cdCardTraits = singleton Dunwich
    , cdVictoryPoints = Just 2
    }

dunwichVillage_242 :: CardDef
dunwichVillage_242 =
  (location "02242" "Dunwich Village" UndimensionedAndUnseen)
    { cdCardTraits = setFromList [Dunwich]
    }

dunwichVillage_243 :: CardDef
dunwichVillage_243 =
  (location "02243" "Dunwich Village" UndimensionedAndUnseen)
    { cdCardTraits = setFromList [Dunwich]
    }

coldSpringGlen_244 :: CardDef
coldSpringGlen_244 =
  (location "02244" "Cold Spring Glen" UndimensionedAndUnseen)
    { cdCardTraits = setFromList [Dunwich]
    }

coldSpringGlen_245 :: CardDef
coldSpringGlen_245 =
  (location "02245" "Cold Spring Glen" UndimensionedAndUnseen)
    { cdCardTraits = setFromList [Dunwich]
    }

tenAcreMeadow_246 :: CardDef
tenAcreMeadow_246 =
  (location "02246" "Ten-Acre Meadow" UndimensionedAndUnseen)
    { cdCardTraits = setFromList [Dunwich]
    }

tenAcreMeadow_247 :: CardDef
tenAcreMeadow_247 =
  (location "02247" "Ten-Acre Meadow" UndimensionedAndUnseen)
    { cdCardTraits = setFromList [Dunwich]
    }

blastedHeath_248 :: CardDef
blastedHeath_248 =
  (location "02248" "Blasted Heath" UndimensionedAndUnseen)
    { cdCardTraits = setFromList [Dunwich]
    }

blastedHeath_249 :: CardDef
blastedHeath_249 =
  (location "02249" "Blasted Heath" UndimensionedAndUnseen)
    { cdCardTraits = setFromList [Dunwich]
    }

whateleyRuins_250 :: CardDef
whateleyRuins_250 =
  (location "02250" "Whateley Ruins" UndimensionedAndUnseen)
    { cdCardTraits = setFromList [Dunwich]
    }

whateleyRuins_251 :: CardDef
whateleyRuins_251 =
  (location "02251" "Whateley Ruins" UndimensionedAndUnseen)
    { cdCardTraits = setFromList [Dunwich]
    }

devilsHopYard_252 :: CardDef
devilsHopYard_252 =
  (location "02252" "Devil's Hop Yard" UndimensionedAndUnseen)
    { cdCardTraits = setFromList [Dunwich]
    }

devilsHopYard_253 :: CardDef
devilsHopYard_253 =
  (location "02253" "Devil's Hop Yard" UndimensionedAndUnseen)
    { cdCardTraits = setFromList [Dunwich]
    }

baseOfTheHill :: CardDef
baseOfTheHill =
  (location "02282" "Base of the Hill" WhereDoomAwaits)
    { cdCardTraits = setFromList [Dunwich, SentinelHill]
    }

ascendingPath :: CardDef
ascendingPath =
  (location "02283" "Ascending Path" WhereDoomAwaits)
    { cdCardTraits = setFromList [Dunwich, SentinelHill]
    }

sentinelPeak :: CardDef
sentinelPeak =
  (location "02284" "Sentinel Peak" WhereDoomAwaits)
    { cdCardTraits = setFromList [Dunwich, SentinelHill]
    , cdVictoryPoints = Just 2
    }

slaugteredWoods :: CardDef
slaugteredWoods =
  (location "02285" "Slaughtered Woods" WhereDoomAwaits)
    { cdCardTraits = setFromList [Dunwich, Woods]
    }

eerieGlade :: CardDef
eerieGlade =
  (location "02286" "Eerie Glade" WhereDoomAwaits)
    { cdCardTraits = setFromList [Dunwich, Woods]
    }

destroyedPath :: CardDef
destroyedPath =
  (location "02287" "Destroyed Path" WhereDoomAwaits)
    { cdCardTraits = setFromList [Dunwich, Woods]
    }

frozenSpring :: CardDef
frozenSpring =
  (location "02288" "Frozen Spring" WhereDoomAwaits)
    { cdCardTraits = setFromList [Dunwich, Woods]
    }

dimensionalGap :: CardDef
dimensionalGap =
  (location "02289" "Dimensional Gap" WhereDoomAwaits)
    { cdCardTraits = setFromList [Dunwich, Woods, Altered]
    }

aTearInThePath :: CardDef
aTearInThePath =
  (location "02290" "A Tear in the Path" WhereDoomAwaits)
    { cdCardTraits = setFromList [Dunwich, Woods, Altered]
    }

uprootedWoods :: CardDef
uprootedWoods =
  (location "02291" "Uprooted Woods" WhereDoomAwaits)
    { cdCardTraits = setFromList [Dunwich, Woods, Altered]
    }

lostMemories :: CardDef
lostMemories =
  (location "02292" "Lost Memories" WhereDoomAwaits)
    { cdCardTraits = setFromList [Dunwich, Woods, Altered]
    }

anotherDimension :: CardDef
anotherDimension =
  (location "02320" ("Another Dimension" <:> "Unfettered by Reality") LostInTimeAndSpace)
    { cdCardTraits = setFromList [Otherworld]
    }

theEdgeOfTheUniverse :: CardDef
theEdgeOfTheUniverse =
  (location "02321" "The Edge of the Universe" LostInTimeAndSpace)
    { cdCardTraits = setFromList [Otherworld]
    }

tearThroughTime :: CardDef
tearThroughTime =
  (location "02322" "Tear Through Time" LostInTimeAndSpace)
    { cdCardTraits = setFromList [Otherworld]
    }

tearThroughSpace :: CardDef
tearThroughSpace =
  (location "02324" "Tear Through Space" LostInTimeAndSpace)
    { cdCardTraits = setFromList [Otherworld, Extradimensional]
    , cdKeywords = setFromList [Keyword.Surge]
    }

prismaticCascade :: CardDef
prismaticCascade =
  (location "02325" "Prismatic Cascade" LostInTimeAndSpace)
    { cdCardTraits = setFromList [Otherworld, Extradimensional]
    }

endlessBridge :: CardDef
endlessBridge = (location "02326" "Endless Bridge" LostInTimeAndSpace)
  { cdCardTraits = setFromList [Otherworld, Extradimensional]
  }

stepsOfYhagharl :: CardDef
stepsOfYhagharl =
  (location "02327" "Steps of Y'hagharl" LostInTimeAndSpace)
    { cdCardTraits = setFromList [Otherworld, Extradimensional]
    }

dimensionalDoorway :: CardDef
dimensionalDoorway =
  (location "02328" "Dimensional Doorway" LostInTimeAndSpace)
    { cdCardTraits = setFromList [Otherworld, Extradimensional]
    }

studyAberrantGateway :: CardDef
studyAberrantGateway = location "50013" ("Study" <:> "Aberrant Gateway") ReturnToTheGathering

guestHall :: CardDef
guestHall = location "50014" "Guest Hall" ReturnToTheGathering

bedroom :: CardDef
bedroom = location "50015" "Bedroom" ReturnToTheGathering

bathroom :: CardDef
bathroom = location "50016" "Bathroom" ReturnToTheGathering

holeInTheWall :: CardDef
holeInTheWall = location "50017" "Hallway" ReturnToTheGathering

returnToAttic :: CardDef
returnToAttic = location "50018" "Attic" ReturnToTheGathering

farAboveYourHouse :: CardDef
farAboveYourHouse =
  (location "50019" "Field of Graves" ReturnToTheGathering)
    { cdVictoryPoints = Just 1
    }

returnToCellar :: CardDef
returnToCellar = location "50020" "Cellar" ReturnToTheGathering

deepBelowYourHouse :: CardDef
deepBelowYourHouse =
  (location "50021" "Ghoul Pits" ReturnToTheGathering)
    { cdVictoryPoints = Just 1
    }

easttownArkhamPoliceStation :: CardDef
easttownArkhamPoliceStation =
  (location "50027" ("Easttown" <:> "Arkham Police Station") ReturnToTheMidnightMasks)
    { cdCardTraits = setFromList [Arkham]
    , cdVictoryPoints = Just 1
    }

northsideTrainStation :: CardDef
northsideTrainStation =
  (location "50028" ("Northside" <:> "Train Station") ReturnToTheMidnightMasks)
    { cdCardTraits = setFromList [Arkham]
    }

miskatonicUniversityMiskatonicMuseum :: CardDef
miskatonicUniversityMiskatonicMuseum =
  (location "50029" ("Miskatonic University" <:> "Miskatonic Museum") ReturnToTheMidnightMasks)
    { cdCardTraits = setFromList [Arkham]
    }

rivertownAbandonedWarehouse :: CardDef
rivertownAbandonedWarehouse =
  (location "50030" ("Rivertown" <:> "Abandoned Warehouse") ReturnToTheMidnightMasks)
    { cdCardTraits = setFromList [Arkham, Central]
    }

arkhamWoodsGreatWillow :: CardDef
arkhamWoodsGreatWillow =
  (location "50033" ("Arkham Woods" <:> "Great Willow") ReturnToTheDevourerBelow)
    { cdCardTraits = setFromList [Woods]
    }

arkhamWoodsLakeside :: CardDef
arkhamWoodsLakeside =
  (location "50034" ("Arkham Woods" <:> "Lakeside") ReturnToTheDevourerBelow)
    { cdCardTraits = setFromList [Woods]
    }

arkhamWoodsCorpseRiddenClearing :: CardDef
arkhamWoodsCorpseRiddenClearing =
  (location "50035" ("Arkham Woods" <:> "Corpse-Ridden Clearing") ReturnToTheDevourerBelow)
    { cdCardTraits = setFromList [Woods]
    }

arkhamWoodsWoodenBridge :: CardDef
arkhamWoodsWoodenBridge =
  (location "50036" ("Arkham Woods" <:> "Wooden Bridge") ReturnToTheDevourerBelow)
    { cdCardTraits = setFromList [Woods]
    }

cursedShores :: CardDef
cursedShores =
  (location "81007" "Cursed Shores" TheBayou)
    { cdCardTraits = setFromList [NewOrleans, Bayou]
    }

gardenDistrict :: CardDef
gardenDistrict =
  (location "81008" "Garden District" TheBayou)
    { cdCardTraits = setFromList [NewOrleans]
    }

broadmoor :: CardDef
broadmoor =
  (location "81009" "Broadmoor" TheBayou)
    { cdCardTraits = setFromList [NewOrleans]
    , cdVictoryPoints = Just 1
    }

brackishWaters :: CardDef
brackishWaters =
  (location "81010" "Brackish Waters" TheBayou)
    { cdCardTraits = setFromList [Riverside, Bayou]
    }

audubonPark :: CardDef
audubonPark =
  (location "81011" "Audubon Park" TheBayou)
    { cdCardTraits = setFromList [Riverside]
    , cdVictoryPoints = Just 1
    }

fauborgMarigny :: CardDef
fauborgMarigny =
  (location "81012" "Fauborg Marigny" TheBayou)
    { cdCardTraits = setFromList [Riverside]
    }

forgottenMarsh :: CardDef
forgottenMarsh =
  (location "81013" "Forgotten Marsh" TheBayou)
    { cdCardTraits = setFromList [Wilderness, Bayou]
    }

trappersCabin :: CardDef
trappersCabin =
  (location "81014" "Trapper's Cabin" TheBayou)
    { cdCardTraits = setFromList [Wilderness]
    }

twistedUnderbrush :: CardDef
twistedUnderbrush =
  (location "81015" "Twisted Underbrush" TheBayou)
    { cdCardTraits = setFromList [Wilderness]
    , cdVictoryPoints = Just 1
    }

foulSwamp :: CardDef
foulSwamp =
  (location "81016" "Foul Swamp" TheBayou)
    { cdCardTraits = setFromList [Unhallowed, Bayou]
    }

ritualGrounds :: CardDef
ritualGrounds =
  (location "81017" "Ritual Grounds" TheBayou)
    { cdCardTraits = setFromList [Unhallowed]
    , cdVictoryPoints = Just 1
    }

overgrownCairns :: CardDef
overgrownCairns =
  (location "81018" "Overgrown Cairns" TheBayou)
    { cdCardTraits = setFromList [Unhallowed]
    }
