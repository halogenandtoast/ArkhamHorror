module Arkham.Location.Cards where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet hiding ( Dunwich )
import Arkham.Keyword qualified as Keyword
import Arkham.LocationSymbol
import Arkham.Name
import Arkham.Trait

locationWithUnrevealed
  :: CardCode
  -> Name
  -> [Trait]
  -> LocationSymbol
  -> [LocationSymbol]
  -> Name
  -> [Trait]
  -> LocationSymbol
  -> [LocationSymbol]
  -> EncounterSet
  -> CardDef 'LocationType
locationWithUnrevealed cardCode unrevealedName unrevealedTraits unrevealedLocationSymbol unrevealedConnectedLocationSymbols name traits locationSymbol connectedLocationSymbols encounterSet
  = (location
      cardCode
      unrevealedName
      unrevealedTraits
      unrevealedLocationSymbol
      unrevealedConnectedLocationSymbols
      encounterSet
    )
    { cdRevealedName = Just name
    , cdRevealedCardTraits = setFromList traits
    , cdLocationRevealedSymbol = Just locationSymbol
    , cdLocationRevealedConnections = connectedLocationSymbols
    }

location
  :: CardCode
  -> Name
  -> [Trait]
  -> LocationSymbol
  -> [LocationSymbol]
  -> EncounterSet
  -> CardDef 'LocationType
location cardCode name traits locationSymbol connectedLocationSymbols encounterSet
  = CardDef
    { cdCardCode = cardCode
    , cdName = name
    , cdRevealedName = Just name
    , cdCost = Nothing
    , cdAdditionalCost = Nothing
    , cdLevel = 0
    , cdCardSubType = Nothing
    , cdClassSymbols = mempty
    , cdSkills = mempty
    , cdCardTraits = setFromList traits
    , cdRevealedCardTraits = setFromList traits
    , cdKeywords = mempty
    , cdFastWindow = Nothing
    , cdActions = []
    , cdRevelation = False
    , cdVictoryPoints = Nothing
    , cdVengeancePoints = Nothing
    , cdCriteria = mempty
    , cdOverrideActionPlayableIfCriteriaMet = False
    , cdCommitRestrictions = mempty
    , cdAttackOfOpportunityModifiers = mempty
    , cdPermanent = False
    , cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Just 1
    , cdUnique = False
    , cdDoubleSided = True
    , cdLimits = []
    , cdExceptional = False
    , cdUses = NoUses
    , cdPlayableFromDiscard = False
    , cdStage = Nothing
    , cdSlots = []
    , cdCardInHandEffects = False
    , cdCardInDiscardEffects = False
    , cdCardInSearchEffects = False
    , cdAlternateCardCodes = []
    , cdArt = unCardCode cardCode
    , cdLocationSymbol = Just locationSymbol
    , cdLocationRevealedSymbol = Just locationSymbol
    , cdLocationConnections = connectedLocationSymbols
    , cdLocationRevealedConnections = connectedLocationSymbols
    , cdPurchaseMentalTrauma = Nothing
    , cdCanReplace = True
    }

allLocationCards :: HashMap CardCode (CardDef 'LocationType)
allLocationCards = mapFromList $ map
  (toCardCode &&& id)
  [ aPocketInTime
  , aTearInThePath
  , abandonedSite
  , abbeyChurch
  , abbeyTowerSpiresForbidden
  , abbeyTowerThePathIsOpen
  , accademiaBridge
  , administrationBuilding
  , administrationOffice_130
  , administrationOffice_131
  , alchemyLabs
  , ancientHall
  , anotherDimension
  , arkhamPoliceStation
  , arkhamWoodsCliffside
  , arkhamWoodsCorpseRiddenClearing
  , arkhamWoodsGreatWillow
  , arkhamWoodsLakeside
  , arkhamWoodsOldHouse
  , arkhamWoodsQuietGlade
  , arkhamWoodsTangledThicket
  , arkhamWoodsTwistingPaths
  , arkhamWoodsUnhallowedGround
  , arkhamWoodsWoodenBridge
  , artGallery
  , ascendingPath
  , asylumHallsWesternPatientWing_168
  , asylumHallsWesternPatientWing_169
  , asylumHallsEasternPatientWing_170
  , asylumHallsEasternPatientWing_171
  , atlantis
  , attic
  , audubonPark
  , backAlley
  , backstage
  , balcony
  , ballroom
  , baseOfTheHill
  , basementHall
  , bathroom
  , bedroom
  , bishopsBrook_202
  , bishopsBrook_203
  , blackCave
  , blastedHeath_248
  , blastedHeath_249
  , bleakPlainsBleakDesolation
  , bleakPlainsStarsOfAldebaran
  , blockedPassage
  , boneFilledCaverns
  , boxOffice
  , brackishWaters
  , bridgeOfSighs
  , bridgeOverNKai
  , brightCanyon
  , broadmoor
  , brokenPassage
  , brokenSteps_289
  , brokenSteps_290
  , burialPit
  , burnedRuins_204
  , burnedRuins_205
  , canalSaintMartin
  , canalSide
  , canalsOfTenochtitlan_180
  , canalsOfTenochtitlan_181
  , candlelitTunnels
  , cavernsOfYoth
  , cellar
  , chamberOfTime
  , chapelOfStAubertThePathIsOpen
  , chapelOfStAubertWatersForbidden
  , chapultepecHill_178
  , chapultepecHill_179
  , chapultepecPark
  , choeurGothique_292
  , choeurGothique_293
  , circuitousTrail
  , cityOfTheSerpents
  , cityOfTheUnseen
  , cloister
  , cloverClubBar
  , cloverClubCardroom
  , cloverClubLounge
  , coldSpringGlen_244
  , coldSpringGlen_245
  , congregationalChurch_208
  , congregationalChurch_209
  , courtyard
  , coyoacan
  , crumblingPrecipice
  , cryptOfTheSepulchralLamp
  , crystalPillars
  , curiositieShoppe
  , cursedShores
  , darkHollow
  , darkenedHall
  , darkSpires
  , deconstructionRoom
  , deepBelowYourHouse
  , depthsOfDemheTheHeightOfTheDepths
  , depthsOfDemheStepsOfThePalace
  , descentToYoth
  , destroyedPath
  , devilsHopYard_252
  , devilsHopYard_253
  , dimensionalDoorway
  , dimensionalGap
  , dimStreetsMappingTheStreets
  , dimStreetsTheKingsParade
  , dimStreetsTheArchway
  , diningCar
  , diningRoom
  , dormitories
  , downtownArkhamAsylum
  , downtownFirstBankOfArkham
  , dressingRoom
  , dunwichVillage_242
  , dunwichVillage_243
  , easttown
  , easttownArkhamPoliceStation
  , eerieGlade
  , endlessBridge
  , engineCar_175
  , engineCar_176
  , engineCar_177
  , entryHall
  , entryway
  , exhibitHallAthabaskanExhibit
  , exhibitHallEgyptianExhibit
  , exhibitHallHallOfTheDead
  , exhibitHallMedusaExhibit
  , exhibitHallNatureExhibit
  , exhibitHallRestrictedHall
  , expeditionCamp
  , eztliExhibit
  , facultyOfficesTheHourIsLate
  , facultyOfficesTheNightIsStillYoung
  , farAboveYourHouse
  , fauborgMarigny
  , floodedSquare
  , forgottenMarsh
  , forkedPath
  , foulSwamp
  , foyer
  , frozenSpring
  , gallery
  , garden
  , gardenDistrict
  , gardensOfLuxembourg
  , gareDOrsay
  , gondola
  , grandChamber
  , grandGuignol
  , grandRue
  , graveyard
  , greatLibrary
  , greenRoom
  , guestHall
  , hallOfHeresy
  , hallOfIdolatry
  , hallsOfPnakotusEasternCorridors
  , hallsOfPnakotusNorthernCorridors
  , hallsOfPnakotusWesternCorridors
  , hallway
  , hiddenLibrary
  , historicalSocietyHistoricalLibrary_133
  , historicalSocietyHistoricalLibrary_136
  , historicalSocietyHistoricalMuseum_130
  , historicalSocietyHistoricalMuseum_132
  , historicalSocietyMeetingRoom
  , historicalSocietyPeabodysOffice
  , historicalSocietyReadingRoom
  , historicalSocietyRecordOffice_129
  , historicalSocietyRecordOffice_138
  , holeInTheWall
  , houseInTheReeds_210
  , houseInTheReeds_211
  , humanitiesBuilding
  , infirmary
  , interviewRoomArrivalChamber
  , interviewRoomIchorFilledChamber
  , interviewRoomRestrainingChamber
  , kitchen
  , knightsHall
  , laBellaLuna
  , laboratoryOfTheGreatRace
  , labyrinthOfBones
  , lakeXochimilco_182
  , lakeXochimilco_183
  , leMarais217
  , leMarais218
  , lightingBox
  , livingRoom
  , lobby
  , lostMemories
  , mainPath
  , messHall
  , metropolitanCathedral
  , miskatonicQuad
  , miskatonicUniversity
  , miskatonicUniversityMiskatonicMuseum
  , montmartre209
  , montmartre210
  , montparnasse
  , mouthOfKnYanTheCavernsMaw
  , mouthOfKnYanTheDepthsBelow
  , mu
  , museumEntrance
  , museumHalls
  , narrowShaft
  , nexusOfNKai
  , northside
  , northsideTrainStation
  , northTower_287
  , northTower_288
  , notreDame
  , operaGarnier212
  , operaGarnier213
  , orneLibrary
  , osbornsGeneralStore_206
  , osbornsGeneralStore_207
  , outerWall_285
  , outerWall_286
  , overgrownCairns
  , overgrownRuins
  , palaceOfTheKing
  , parlor
  , parlorCar
  , passengerCar_167
  , passengerCar_168
  , passengerCar_169
  , passengerCar_170
  , passengerCar_171
  , pathOfThorns
  , patientConfinementDanielsCell
  , patientConfinementOccupiedCell
  , patientConfinementDrearyCell
  , patientConfinementFamiliarCell
  , pereLachaiseCemetery
  , perilousGulch
  , plateauOfLeng
  , pnakotus
  , porteDeLAvancee
  , prismaticCascade
  , quietHalls_131
  , quietHalls_135
  , rehearsalRoom
  , returnToAttic
  , returnToCellar
  , rialtoBridge
  , ritualGrounds
  , ritualSite
  , rivertown
  , rivertownAbandonedWarehouse
  , riverCanyon
  , ropeBridge
  , ruinsOfCarcosaAMomentsRest
  , ruinsOfCarcosaInhabitantOfCarcosa
  , ruinsOfCarcosaTheCoffin
  , ruinsOfEztli
  , ruinsOfNewYork
  , sacredWoods_184
  , sacredWoods_185
  , sanMarcoBasilica
  , schoolhouse_212
  , schoolhouse_213
  , scienceBuilding
  , secretPassage
  , securityOffice_128
  , securityOffice_129
  , sentinelPeak
  , serpentsHaven
  , shiveringPools
  , shoresOfHali
  , shoresOfRlyeh
  , slaughteredWoods
  , sleepingCar
  , southsideHistoricalSociety
  , southsideMasBoardingHouse
  , stMarysHospital
  , stepsOfYhagharl
  , stepsOfYoth
  , stoneAltar
  , stoneArchways
  , streetsOfVenice
  , studentUnion
  , study
  , studyAberrantGateway
  , tearThroughSpace
  , tearThroughTime
  , templeOfTheFang
  , templeRuins
  , templesOfTenochtitlan_176
  , templesOfTenochtitlan_177
  , temploMayor_174
  , temploMayor_175
  , tenAcreMeadow_246
  , tenAcreMeadow_247
  , theEdgeOfTheUniverse
  , theGateToHell
  , theGuardian
  , theHiddenChamber
  , theatre
  , timeWrackedWoods
  , tombOfShadows
  , towersOfPnakotus
  , townHall
  , trainTracks
  , trapRoom
  , trappersCabin
  , twistedUnderbrush
  , undergroundRuins
  , uprootedWoods
  , valusia
  , vastPassages
  , velmasDiner
  , venetianGarden
  , villageCommons
  , vipArea
  , wellOfSouls
  , whateleyRuins_250
  , whateleyRuins_251
  , xochimilco
  , yard
  , yithianOrrery
  , yourHouse
  , yuggoth
  , zocalo
  ]

allSpecialLocationCards :: HashMap CardCode (CardDef 'LocationType)
allSpecialLocationCards =
  mapFromList $ map (toCardCode &&& id) [betweenWorlds]

vengeance :: Int -> CardDef k -> CardDef k
vengeance n def = def { cdVengeancePoints = Just n }

victory :: Int -> CardDef k -> CardDef k
victory n def = def { cdVictoryPoints = Just n }

singleSided :: CardDef k -> CardDef k
singleSided def = def { cdDoubleSided = False }

quantity :: Int -> CardDef k -> CardDef k
quantity n def = def { cdEncounterSetQuantity = Just n }

study :: CardDef 'LocationType
study = location "01111" "Study" mempty Circle [] TheGathering

hallway :: CardDef 'LocationType
hallway = location
  "01112"
  "Hallway"
  mempty
  Square
  [Triangle, Plus, Diamond]
  TheGathering

attic :: CardDef 'LocationType
attic =
  victory 1 $ location "01113" "Attic" mempty Triangle [Square] TheGathering

cellar :: CardDef 'LocationType
cellar =
  victory 1 $ location "01114" "Cellar" mempty Plus [Square] TheGathering

parlor :: CardDef 'LocationType
parlor = location "01115" "Parlor" mempty Diamond [Square] TheGathering

yourHouse :: CardDef 'LocationType
yourHouse =
  location "01124" "Your House" [Arkham] Squiggle [Circle] TheMidnightMasks

rivertown :: CardDef 'LocationType
rivertown = location
  "01125"
  "Rivertown"
  [Arkham, Central]
  Circle
  [Moon, Diamond, Square, Squiggle, Hourglass]
  TheMidnightMasks

southsideHistoricalSociety :: CardDef 'LocationType
southsideHistoricalSociety = location
  "01126"
  ("Southside" <:> "Historical Society")
  [Arkham]
  Square
  [Triangle, Plus, Circle]
  TheMidnightMasks

southsideMasBoardingHouse :: CardDef 'LocationType
southsideMasBoardingHouse = location
  "01127"
  ("Southside" <:> "Ma's Boarding House")
  [Arkham]
  Square
  [Triangle, Plus, Circle]
  TheMidnightMasks

stMarysHospital :: CardDef 'LocationType
stMarysHospital = location
  "01128"
  "St. Mary's Hospital"
  [Arkham]
  Plus
  [Diamond, Square]
  TheMidnightMasks

miskatonicUniversity :: CardDef 'LocationType
miskatonicUniversity = victory 1 $ location
  "01129"
  "Miskatonic University"
  [Arkham]
  Diamond
  [T, Plus, Circle, Square]
  TheMidnightMasks

downtownFirstBankOfArkham :: CardDef 'LocationType
downtownFirstBankOfArkham = location
  "01130"
  ("Downtown" <:> "First Bank of Arkham")
  [Arkham]
  Triangle
  [Moon, T]
  TheMidnightMasks

downtownArkhamAsylum :: CardDef 'LocationType
downtownArkhamAsylum = victory 1 $ location
  "01131"
  ("Downtown" <:> "Arkham Asylum")
  [Arkham]
  Triangle
  [Moon, T]
  TheMidnightMasks

easttown :: CardDef 'LocationType
easttown =
  location "01132" "Easttown" [Arkham] Moon [Circle, Triangle] TheMidnightMasks

graveyard :: CardDef 'LocationType
graveyard = victory 1
  $ location "01133" "Graveyard" [Arkham] Hourglass [Circle] TheMidnightMasks

northside :: CardDef 'LocationType
northside = victory 1 $ location
  "01134"
  "Northside"
  [Arkham]
  T
  [Diamond, Triangle]
  TheMidnightMasks

mainPath :: CardDef 'LocationType
mainPath =
  location "01149" "Main Path" [Woods] Squiggle [Square, Plus] TheDevourerBelow

arkhamWoodsUnhallowedGround :: CardDef 'LocationType
arkhamWoodsUnhallowedGround = locationWithUnrevealed
  "01150"
  "Arkham Woods"
  [Woods]
  Square
  [Squiggle]
  ("Arkham Woods" <:> "Unhallowed Ground")
  [Woods]
  Triangle
  [Squiggle, Hourglass, Diamond]
  TheDevourerBelow

arkhamWoodsTwistingPaths :: CardDef 'LocationType
arkhamWoodsTwistingPaths = locationWithUnrevealed
  "01151"
  "Arkham Woods"
  [Woods]
  Square
  [Squiggle]
  ("Arkham Woods" <:> "Twisting Paths")
  [Woods]
  T
  [Squiggle, Diamond, Equals]
  TheDevourerBelow

arkhamWoodsOldHouse :: CardDef 'LocationType
arkhamWoodsOldHouse = locationWithUnrevealed
  "01152"
  "Arkham Woods"
  [Woods]
  Square
  [Squiggle]
  ("Arkham Woods" <:> "Old House")
  [Woods]
  Diamond
  [Squiggle, Triangle, T]
  TheDevourerBelow

arkhamWoodsCliffside :: CardDef 'LocationType
arkhamWoodsCliffside = locationWithUnrevealed
  "01153"
  "Arkham Woods"
  [Woods]
  Square
  [Squiggle]
  ("Arkham Woods" <:> "Cliffside")
  [Woods]
  Hourglass
  [Squiggle, Moon, Triangle]
  TheDevourerBelow

arkhamWoodsTangledThicket :: CardDef 'LocationType
arkhamWoodsTangledThicket = locationWithUnrevealed
  "01154"
  "Arkham Woods"
  [Woods]
  Square
  [Squiggle]
  ("Arkham Woods" <:> "Tangled Thicket")
  [Woods]
  Equals
  [Squiggle, T, Moon]
  TheDevourerBelow

arkhamWoodsQuietGlade :: CardDef 'LocationType
arkhamWoodsQuietGlade = locationWithUnrevealed
  "01155"
  "Arkham Woods"
  [Woods]
  Square
  [Squiggle]
  ("Arkham Woods" <:> "Quiet Glade")
  [Woods]
  Moon
  [Squiggle, Equals, Hourglass]
  TheDevourerBelow

ritualSite :: CardDef 'LocationType
ritualSite =
  location "01156" "Ritual Site" [Cave] Plus [Squiggle] TheDevourerBelow

miskatonicQuad :: CardDef 'LocationType
miskatonicQuad = location
  "02048"
  "Miskatonic Quad"
  [Miskatonic]
  Plus
  [Triangle, Hourglass, Square, Diamond, Circle]
  ExtracurricularActivity

humanitiesBuilding :: CardDef 'LocationType
humanitiesBuilding = location
  "02049"
  "Humanities Building"
  [Miskatonic]
  Square
  [Plus, Triangle]
  ExtracurricularActivity

orneLibrary :: CardDef 'LocationType
orneLibrary = victory 1 $ location
  "02050"
  "Orne Library"
  [Miskatonic]
  Triangle
  [Plus, Square]
  ExtracurricularActivity

studentUnion :: CardDef 'LocationType
studentUnion = location
  "02051"
  "Student Union"
  [Miskatonic]
  Diamond
  [Plus, Equals]
  ExtracurricularActivity

dormitories :: CardDef 'LocationType
dormitories = victory 1 $ location
  "02052"
  "Dormitories"
  [Miskatonic]
  Equals
  [Diamond]
  ExtracurricularActivity

administrationBuilding :: CardDef 'LocationType
administrationBuilding = location
  "02053"
  "Administration Building"
  [Miskatonic]
  Circle
  [Plus, T]
  ExtracurricularActivity

facultyOfficesTheNightIsStillYoung :: CardDef 'LocationType
facultyOfficesTheNightIsStillYoung = victory 1 $ location
  "02054"
  ("Faculty Offices" <:> "The Night is Still Young")
  [Miskatonic]
  T
  [Circle]
  ExtracurricularActivity

facultyOfficesTheHourIsLate :: CardDef 'LocationType
facultyOfficesTheHourIsLate = location
  "02055"
  ("Faculty Offices" <:> "The Hour is Late")
  [Miskatonic]
  T
  [Circle]
  ExtracurricularActivity

scienceBuilding :: CardDef 'LocationType
scienceBuilding = location
  "02056"
  "Science Building"
  [Miskatonic]
  Hourglass
  [Plus, Squiggle]
  ExtracurricularActivity

alchemyLabs :: CardDef 'LocationType
alchemyLabs = location
  "02057"
  "Alchemy Labs"
  [Miskatonic]
  Squiggle
  [Hourglass]
  ExtracurricularActivity

laBellaLuna :: CardDef 'LocationType
laBellaLuna =
  location "02070" "La Bella Luna" [Arkham] Moon [Circle] TheHouseAlwaysWins

cloverClubLounge :: CardDef 'LocationType
cloverClubLounge = location
  "02071"
  "Clover Club Lounge"
  [CloverClub]
  Circle
  [Moon, Square, Triangle]
  TheHouseAlwaysWins

cloverClubBar :: CardDef 'LocationType
cloverClubBar = location
  "02072"
  "Clover Club Bar"
  [CloverClub]
  Square
  [Triangle, Circle]
  TheHouseAlwaysWins

cloverClubCardroom :: CardDef 'LocationType
cloverClubCardroom = location
  "02073"
  "Clover Club Cardroom"
  [CloverClub]
  Triangle
  [Circle, Square, Diamond]
  TheHouseAlwaysWins

darkenedHall :: CardDef 'LocationType
darkenedHall = location
  "02074"
  "Darkened Hall"
  [CloverClub]
  Diamond
  [Triangle, T, Hourglass, Plus, Squiggle]
  TheHouseAlwaysWins

artGallery :: CardDef 'LocationType
artGallery = victory 1 $ locationWithUnrevealed
  "02075"
  "Back Hall Doorway"
  [CloverClub]
  T
  [Diamond]
  "Art Gallery"
  [CloverClub]
  Hourglass
  [Diamond]
  TheHouseAlwaysWins

vipArea :: CardDef 'LocationType
vipArea = victory 1 $ locationWithUnrevealed
  "02076"
  "Back Hall Doorway"
  [CloverClub]
  T
  [Diamond]
  "VIP Area"
  [CloverClub]
  Plus
  [Diamond]
  TheHouseAlwaysWins

backAlley :: CardDef 'LocationType
backAlley = victory 1 $ locationWithUnrevealed
  "02077"
  "Back Hall Doorway"
  [CloverClub]
  T
  [Diamond]
  "Back Alley"
  [CloverClub]
  Squiggle
  [Diamond]
  TheHouseAlwaysWins

museumEntrance :: CardDef 'LocationType
museumEntrance = location
  "02126"
  "Museum Entrance"
  [Miskatonic]
  Circle
  [Square]
  TheMiskatonicMuseum

museumHalls :: CardDef 'LocationType
museumHalls = location
  "02127"
  "Museum Halls"
  [Miskatonic]
  Square
  [Circle, Diamond, Triangle]
  TheMiskatonicMuseum

securityOffice_128 :: CardDef 'LocationType
securityOffice_128 = location
  "02128"
  "Security Office"
  [Miskatonic]
  Diamond
  [Square]
  TheMiskatonicMuseum

securityOffice_129 :: CardDef 'LocationType
securityOffice_129 = location
  "02129"
  "Security Office"
  [Miskatonic]
  Diamond
  [Square]
  TheMiskatonicMuseum

administrationOffice_130 :: CardDef 'LocationType
administrationOffice_130 = location
  "02130"
  "Administration Office"
  [Miskatonic]
  Triangle
  [Square]
  TheMiskatonicMuseum

administrationOffice_131 :: CardDef 'LocationType
administrationOffice_131 = location
  "02131"
  "Administration Office"
  [Miskatonic]
  Triangle
  [Square]
  TheMiskatonicMuseum

exhibitHallAthabaskanExhibit :: CardDef 'LocationType
exhibitHallAthabaskanExhibit = locationWithUnrevealed
  "02132"
  "Exhibit Hall"
  [Miskatonic, Exhibit]
  NoSymbol
  [Square]
  ("Exhibit Hall" <:> "Athabaskan Exhibit")
  [Miskatonic, Exhibit]
  Plus
  [Square]
  TheMiskatonicMuseum

exhibitHallMedusaExhibit :: CardDef 'LocationType
exhibitHallMedusaExhibit = victory 1 $ locationWithUnrevealed
  "02133"
  "Exhibit Hall"
  [Miskatonic, Exhibit]
  NoSymbol
  [Square]
  ("Exhibit Hall" <:> "Medusa Exhibit")
  [Miskatonic, Exhibit]
  T
  [Square, Moon]
  TheMiskatonicMuseum

exhibitHallNatureExhibit :: CardDef 'LocationType
exhibitHallNatureExhibit = victory 1 $ locationWithUnrevealed
  "02134"
  "Exhibit Hall"
  [Miskatonic, Exhibit]
  NoSymbol
  [Square]
  ("Exhibit Hall" <:> "Nature Exhibit")
  [Miskatonic, Exhibit]
  Hourglass
  [Square, Squiggle]
  TheMiskatonicMuseum

exhibitHallEgyptianExhibit :: CardDef 'LocationType
exhibitHallEgyptianExhibit = victory 1 $ locationWithUnrevealed
  "02135"
  "Exhibit Hall"
  [Miskatonic, Exhibit]
  NoSymbol
  [Square]
  ("Exhibit Hall" <:> "Egyptian Exhibit")
  [Miskatonic, Exhibit]
  Moon
  [Square, T]
  TheMiskatonicMuseum

exhibitHallHallOfTheDead :: CardDef 'LocationType
exhibitHallHallOfTheDead = victory 1 $ locationWithUnrevealed
  "02136"
  "Exhibit Hall"
  [Miskatonic, Exhibit]
  NoSymbol
  [Square]
  ("Exhibit Hall" <:> "Hall of the Dead")
  [Miskatonic, Exhibit]
  Squiggle
  [Square, Hourglass]
  TheMiskatonicMuseum

exhibitHallRestrictedHall :: CardDef 'LocationType
exhibitHallRestrictedHall = victory 1 $ locationWithUnrevealed
  "02137"
  "Exhibit Hall"
  [Miskatonic, Exhibit]
  NoSymbol
  [Square]
  ("Exhibit Hall" <:> "Restricted Hall")
  [Miskatonic, Exhibit]
  Equals
  [Square]
  TheMiskatonicMuseum

passengerCar_167 :: CardDef 'LocationType
passengerCar_167 = locationWithUnrevealed
  "02167"
  "Train Car"
  [Train]
  NoSymbol
  []
  "Passenger Car"
  [Train]
  NoSymbol
  []
  TheEssexCountyExpress

passengerCar_168 :: CardDef 'LocationType
passengerCar_168 = locationWithUnrevealed
  "02168"
  "Train Car"
  [Train]
  NoSymbol
  []
  "Passenger Car"
  [Train]
  NoSymbol
  []
  TheEssexCountyExpress

passengerCar_169 :: CardDef 'LocationType
passengerCar_169 = locationWithUnrevealed
  "02169"
  "Train Car"
  [Train]
  NoSymbol
  []
  "Passenger Car"
  [Train]
  NoSymbol
  []
  TheEssexCountyExpress

passengerCar_170 :: CardDef 'LocationType
passengerCar_170 = locationWithUnrevealed
  "02170"
  "Train Car"
  [Train]
  NoSymbol
  []
  "Passenger Car"
  [Train]
  NoSymbol
  []
  TheEssexCountyExpress

passengerCar_171 :: CardDef 'LocationType
passengerCar_171 = locationWithUnrevealed
  "02171"
  "Train Car"
  [Train]
  NoSymbol
  []
  "Passenger Car"
  [Train]
  NoSymbol
  []
  TheEssexCountyExpress

sleepingCar :: CardDef 'LocationType
sleepingCar = locationWithUnrevealed
  "02172"
  "Train Car"
  [Train]
  NoSymbol
  []
  "Sleeping Car"
  [Train]
  NoSymbol
  []
  TheEssexCountyExpress

diningCar :: CardDef 'LocationType
diningCar = locationWithUnrevealed
  "02173"
  "Train Car"
  [Train]
  NoSymbol
  []
  "Dining Car"
  [Train]
  NoSymbol
  []
  TheEssexCountyExpress

parlorCar :: CardDef 'LocationType
parlorCar = victory 1 $ locationWithUnrevealed
  "02174"
  "Train Car"
  [Train]
  NoSymbol
  []
  "Parlor Car"
  [Train]
  NoSymbol
  []
  TheEssexCountyExpress

engineCar_175 :: CardDef 'LocationType
engineCar_175 = victory 1
  $ location "02175" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress

engineCar_176 :: CardDef 'LocationType
engineCar_176 = victory 1
  $ location "02176" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress

engineCar_177 :: CardDef 'LocationType
engineCar_177 = victory 1
  $ location "02177" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress

villageCommons :: CardDef 'LocationType
villageCommons = location
  "02201"
  "Village Commons"
  [Dunwich, Central]
  Plus
  [Square, Circle, Moon]
  BloodOnTheAltar

bishopsBrook_202 :: CardDef 'LocationType
bishopsBrook_202 = location
  "02202"
  "Bishop's Brook"
  [Dunwich]
  Square
  [Plus, Circle, Triangle]
  BloodOnTheAltar

bishopsBrook_203 :: CardDef 'LocationType
bishopsBrook_203 = location
  "02203"
  "Bishop's Brook"
  [Dunwich]
  Square
  [Plus, Circle, Triangle]
  BloodOnTheAltar

burnedRuins_204 :: CardDef 'LocationType
burnedRuins_204 = location
  "02204"
  "Burned Ruins"
  [Dunwich]
  Triangle
  [Square, Diamond]
  BloodOnTheAltar

burnedRuins_205 :: CardDef 'LocationType
burnedRuins_205 = location
  "02205"
  "Burned Ruins"
  [Dunwich]
  Triangle
  [Square, Diamond]
  BloodOnTheAltar

osbornsGeneralStore_206 :: CardDef 'LocationType
osbornsGeneralStore_206 = location
  "02206"
  "Osborn's General Store"
  [Dunwich]
  Circle
  [Moon, Square]
  BloodOnTheAltar

osbornsGeneralStore_207 :: CardDef 'LocationType
osbornsGeneralStore_207 = location
  "02207"
  "Osborn's General Store"
  [Dunwich]
  Circle
  [Moon, Square]
  BloodOnTheAltar

congregationalChurch_208 :: CardDef 'LocationType
congregationalChurch_208 = location
  "02208"
  "Congregational Church"
  [Dunwich]
  Diamond
  [Plus, Triangle, Squiggle]
  BloodOnTheAltar

congregationalChurch_209 :: CardDef 'LocationType
congregationalChurch_209 = location
  "02209"
  "Congregational Church"
  [Dunwich]
  Diamond
  [Plus, Triangle, Squiggle]
  BloodOnTheAltar

houseInTheReeds_210 :: CardDef 'LocationType
houseInTheReeds_210 = location
  "02210"
  "House in the Reeds"
  [Dunwich]
  Squiggle
  [Diamond, Moon]
  BloodOnTheAltar

houseInTheReeds_211 :: CardDef 'LocationType
houseInTheReeds_211 = location
  "02211"
  "House in the Reeds"
  [Dunwich]
  Squiggle
  [Diamond, Moon]
  BloodOnTheAltar

schoolhouse_212 :: CardDef 'LocationType
schoolhouse_212 = location
  "02212"
  "Schoolhouse"
  [Dunwich]
  Moon
  [Plus, Squiggle, Moon]
  BloodOnTheAltar

schoolhouse_213 :: CardDef 'LocationType
schoolhouse_213 = location
  "02213"
  "Schoolhouse"
  [Dunwich]
  Moon
  [Plus, Squiggle, Moon]
  BloodOnTheAltar

theHiddenChamber :: CardDef 'LocationType
theHiddenChamber = victory 2 $ location
  "02214"
  ("The Hidden Chamber" <:> "Prison of the Beast")
  [Dunwich]
  NoSymbol
  []
  BloodOnTheAltar

dunwichVillage_242 :: CardDef 'LocationType
dunwichVillage_242 = location
  "02242"
  "Dunwich Village"
  [Dunwich]
  Circle
  [Triangle, Square, Diamond]
  UndimensionedAndUnseen

dunwichVillage_243 :: CardDef 'LocationType
dunwichVillage_243 = location
  "02243"
  "Dunwich Village"
  [Dunwich]
  Circle
  [Triangle, Square, Diamond]
  UndimensionedAndUnseen

coldSpringGlen_244 :: CardDef 'LocationType
coldSpringGlen_244 = location
  "02244"
  "Cold Spring Glen"
  [Dunwich]
  Triangle
  [Circle, Diamond, Plus]
  UndimensionedAndUnseen

coldSpringGlen_245 :: CardDef 'LocationType
coldSpringGlen_245 = location
  "02245"
  "Cold Spring Glen"
  [Dunwich]
  Triangle
  [Circle, Diamond, Plus]
  UndimensionedAndUnseen

tenAcreMeadow_246 :: CardDef 'LocationType
tenAcreMeadow_246 = location
  "02246"
  "Ten-Acre Meadow"
  [Dunwich]
  Diamond
  [Circle, Triangle, Plus]
  UndimensionedAndUnseen

tenAcreMeadow_247 :: CardDef 'LocationType
tenAcreMeadow_247 = location
  "02247"
  "Ten-Acre Meadow"
  [Dunwich]
  Diamond
  [Circle, Triangle, Plus]
  UndimensionedAndUnseen

blastedHeath_248 :: CardDef 'LocationType
blastedHeath_248 = location
  "02248"
  "Blasted Heath"
  [Dunwich]
  Square
  [Circle, Hourglass]
  UndimensionedAndUnseen

blastedHeath_249 :: CardDef 'LocationType
blastedHeath_249 = location
  "02249"
  "Blasted Heath"
  [Dunwich]
  Square
  [Circle, Hourglass]
  UndimensionedAndUnseen

whateleyRuins_250 :: CardDef 'LocationType
whateleyRuins_250 = location
  "02250"
  "Whateley Ruins"
  [Dunwich]
  Plus
  [Triangle, Diamond, Hourglass]
  UndimensionedAndUnseen

whateleyRuins_251 :: CardDef 'LocationType
whateleyRuins_251 = location
  "02251"
  "Whateley Ruins"
  [Dunwich]
  Plus
  [Triangle, Diamond, Hourglass]
  UndimensionedAndUnseen

devilsHopYard_252 :: CardDef 'LocationType
devilsHopYard_252 = location
  "02252"
  "Devil's Hop Yard"
  [Dunwich]
  Hourglass
  [Square, Plus]
  UndimensionedAndUnseen

devilsHopYard_253 :: CardDef 'LocationType
devilsHopYard_253 = location
  "02253"
  "Devil's Hop Yard"
  [Dunwich]
  Hourglass
  [Square, Plus]
  UndimensionedAndUnseen

baseOfTheHill :: CardDef 'LocationType
baseOfTheHill = location
  "02282"
  "Base of the Hill"
  [Dunwich, SentinelHill]
  Triangle
  [Square, Plus, Squiggle, Hourglass]
  WhereDoomAwaits

ascendingPath :: CardDef 'LocationType
ascendingPath = location
  "02283"
  "Ascending Path"
  [Dunwich, SentinelHill]
  Square
  [Triangle, Diamond, T, Equals, Moon]
  WhereDoomAwaits

sentinelPeak :: CardDef 'LocationType
sentinelPeak = victory 2 $ location
  "02284"
  "Sentinel Peak"
  [Dunwich, SentinelHill]
  Diamond
  [Square]
  WhereDoomAwaits

slaughteredWoods :: CardDef 'LocationType
slaughteredWoods = locationWithUnrevealed
  "02285"
  "Diverging Path"
  [Dunwich, Woods]
  NoSymbol
  []
  "Slaughtered Woods"
  [Dunwich, Woods]
  Plus
  [Triangle, Hourglass]
  WhereDoomAwaits

eerieGlade :: CardDef 'LocationType
eerieGlade = locationWithUnrevealed
  "02286"
  "Diverging Path"
  [Dunwich, Woods]
  NoSymbol
  []
  "Eerie Glade"
  [Dunwich, Woods]
  Hourglass
  [Triangle, Plus]
  WhereDoomAwaits

destroyedPath :: CardDef 'LocationType
destroyedPath = locationWithUnrevealed
  "02287"
  "Diverging Path"
  [Dunwich, Woods]
  NoSymbol
  []
  "Destroyed Path"
  [Dunwich, Woods]
  Squiggle
  [Triangle, Equals]
  WhereDoomAwaits

frozenSpring :: CardDef 'LocationType
frozenSpring = locationWithUnrevealed
  "02288"
  "Diverging Path"
  [Dunwich, Woods]
  NoSymbol
  []
  "Frozen Spring"
  [Dunwich, Woods]
  Plus
  [Triangle, Hourglass]
  WhereDoomAwaits

dimensionalGap :: CardDef 'LocationType
dimensionalGap = locationWithUnrevealed
  "02289"
  "Altered Path"
  [Dunwich, Woods, Altered]
  NoSymbol
  []
  "Dimensional Gap"
  [Dunwich, Woods, Altered]
  T
  [Square, Moon]
  WhereDoomAwaits

aTearInThePath :: CardDef 'LocationType
aTearInThePath = locationWithUnrevealed
  "02290"
  "Altered Path"
  [Dunwich, Woods, Altered]
  NoSymbol
  []
  "A Tear in the Path"
  [Dunwich, Woods, Altered]
  Equals
  [Square, Squiggle]
  WhereDoomAwaits

uprootedWoods :: CardDef 'LocationType
uprootedWoods = locationWithUnrevealed
  "02291"
  "Altered Path"
  [Dunwich, Woods, Altered]
  NoSymbol
  []
  "Uprooted Woods"
  [Dunwich, Woods, Altered]
  Moon
  [Square, T]
  WhereDoomAwaits

lostMemories :: CardDef 'LocationType
lostMemories = locationWithUnrevealed
  "02292"
  "Altered Path"
  [Dunwich, Woods, Altered]
  NoSymbol
  []
  "Lost Memories"
  [Dunwich, Woods, Altered]
  T
  [Square, Moon]
  WhereDoomAwaits

anotherDimension :: CardDef 'LocationType
anotherDimension = location
  "02320"
  ("Another Dimension" <:> "Unfettered by Reality")
  [Otherworld]
  Circle
  [Square, Diamond, Triangle]
  LostInTimeAndSpace

theEdgeOfTheUniverse :: CardDef 'LocationType
theEdgeOfTheUniverse = location
  "02321"
  "The Edge of the Universe"
  [Otherworld]
  Moon
  [Plus, Squiggle]
  LostInTimeAndSpace

tearThroughTime :: CardDef 'LocationType
tearThroughTime = location
  "02322"
  "Tear Through Time"
  [Otherworld]
  Moon
  [Circle, Plus, Squiggle]
  LostInTimeAndSpace

tearThroughSpace :: CardDef 'LocationType
tearThroughSpace = (location
                     "02324"
                     "Tear Through Space"
                     [Otherworld, Extradimensional]
                     Square
                     [Diamond, Triangle, Square]
                     LostInTimeAndSpace
                   )
  { cdKeywords = setFromList [Keyword.Surge]
  , cdDoubleSided = False
  , cdEncounterSetQuantity = Just 4
  }

prismaticCascade :: CardDef 'LocationType
prismaticCascade = (location
                     "02325"
                     "Prismatic Cascade"
                     [Otherworld, Extradimensional]
                     Diamond
                     [Square, Plus]
                     LostInTimeAndSpace
                   )
  { cdDoubleSided = False
  , cdEncounterSetQuantity = Just 2
  }

endlessBridge :: CardDef 'LocationType
endlessBridge = (location
                  "02326"
                  "Endless Bridge"
                  [Otherworld, Extradimensional]
                  Triangle
                  [Square, Squiggle]
                  LostInTimeAndSpace
                )
  { cdDoubleSided = False
  , cdEncounterSetQuantity = Just 2
  }

stepsOfYhagharl :: CardDef 'LocationType
stepsOfYhagharl = singleSided $ location
  "02327"
  "Steps of Y'hagharl"
  [Otherworld, Extradimensional]
  Plus
  [Diamond, Moon]
  LostInTimeAndSpace

dimensionalDoorway :: CardDef 'LocationType
dimensionalDoorway = singleSided $ location
  "02328"
  "Dimensional Doorway"
  [Otherworld, Extradimensional]
  Squiggle
  [Triangle, Moon]
  LostInTimeAndSpace

theatre :: CardDef 'LocationType
theatre =
  location "03049" "Theatre" mempty Circle [Diamond, Triangle] CurtainCall

lobby :: CardDef 'LocationType
lobby =
  location "03050" "Lobby" mempty Triangle [Circle, Square, Plus] CurtainCall

balcony :: CardDef 'LocationType
balcony = victory 1
  $ location "03051" "Balcony" mempty Square [Circle, Triangle] CurtainCall

backstage :: CardDef 'LocationType
backstage =
  location "03052" "Backstage" mempty Diamond [Circle, Moon] CurtainCall

lightingBox :: CardDef 'LocationType
lightingBox = victory 1 $ locationWithUnrevealed
  "03053"
  "Lobby Doorway"
  [Private]
  Plus
  [Triangle]
  "Lighting Box"
  [Private]
  Plus
  [Triangle]
  CurtainCall

boxOffice :: CardDef 'LocationType
boxOffice = locationWithUnrevealed
  "03054"
  "Lobby Doorway"
  [Private]
  Plus
  [Triangle]
  "Box Office"
  [Private]
  Plus
  [Triangle]
  CurtainCall

greenRoom :: CardDef 'LocationType
greenRoom = victory 1 $ locationWithUnrevealed
  "03055"
  "Lobby Doorway"
  [Private]
  Plus
  [Triangle]
  "Green Room"
  [Private]
  Plus
  [Triangle]
  CurtainCall

dressingRoom :: CardDef 'LocationType
dressingRoom = locationWithUnrevealed
  "03056"
  "Backstage Doorway"
  [Private]
  Moon
  [Diamond]
  "Dressing Room"
  [Private]
  Moon
  [Diamond]
  CurtainCall

rehearsalRoom :: CardDef 'LocationType
rehearsalRoom = victory 1 $ locationWithUnrevealed
  "03057"
  "Backstage Doorway"
  [Private]
  Moon
  [Diamond]
  "Rehearsal Room"
  [Private]
  Moon
  [Diamond]
  CurtainCall

trapRoom :: CardDef 'LocationType
trapRoom = victory 1 $ locationWithUnrevealed
  "03058"
  "Backstage Doorway"
  [Private]
  Moon
  [Diamond]
  "Trap Room"
  [Private]
  Moon
  [Diamond]
  CurtainCall

foyer :: CardDef 'LocationType
foyer = location "03070" "Foyer" mempty T [Circle, Square, Equals] TheLastKing

ballroom :: CardDef 'LocationType
ballroom =
  location "03071" "Ballroom" mempty Square [T, Circle, Squiggle] TheLastKing

livingRoom :: CardDef 'LocationType
livingRoom =
  location "03072" "Living Room" mempty Equals [T, Circle, Plus] TheLastKing

gallery :: CardDef 'LocationType
gallery = location "03073" "Gallery" mempty Plus [Equals, Circle] TheLastKing

courtyard :: CardDef 'LocationType
courtyard = location
  "03074"
  "Courtyard"
  mempty
  Circle
  [Squiggle, Square, T, Equals, Plus]
  TheLastKing

diningRoom :: CardDef 'LocationType
diningRoom =
  location "03075" "Dining Room" mempty Squiggle [Square, Circle] TheLastKing

entryHall :: CardDef 'LocationType
entryHall =
  location "03127" "Entry Hall" [GroundFloor] Square [Circle] EchoesOfThePast

historicalSocietyMeetingRoom :: CardDef 'LocationType
historicalSocietyMeetingRoom = locationWithUnrevealed
  "03128"
  "Historical Society"
  [GroundFloor]
  NoSymbol
  [Square]
  ("Historical Society" <:> "Meeting Room")
  [GroundFloor, Passageway]
  Diamond
  [Square]
  EchoesOfThePast

historicalSocietyRecordOffice_129 :: CardDef 'LocationType
historicalSocietyRecordOffice_129 = locationWithUnrevealed
  "03129"
  "Historical Society"
  [GroundFloor]
  NoSymbol
  [Square]
  ("Historical Society" <:> "Record Office")
  [GroundFloor]
  Plus
  [Square]
  EchoesOfThePast

historicalSocietyHistoricalMuseum_130 :: CardDef 'LocationType
historicalSocietyHistoricalMuseum_130 = locationWithUnrevealed
  "03130"
  "Historical Society"
  [GroundFloor]
  NoSymbol
  [Square]
  ("Historical Society" <:> "Historical Museum")
  [GroundFloor]
  Heart
  [Square, Hourglass]
  EchoesOfThePast

quietHalls_131 :: CardDef 'LocationType
quietHalls_131 = location
  "03131"
  "Quiet Halls"
  [SecondFloor]
  Circle
  [Square, Star]
  EchoesOfThePast

historicalSocietyHistoricalMuseum_132 :: CardDef 'LocationType
historicalSocietyHistoricalMuseum_132 = locationWithUnrevealed
  "03132"
  "Historical Society"
  [SecondFloor]
  NoSymbol
  [Circle]
  ("Historical Society" <:> "Historical Museum")
  [SecondFloor]
  Hourglass
  [Circle, Heart]
  EchoesOfThePast

historicalSocietyHistoricalLibrary_133 :: CardDef 'LocationType
historicalSocietyHistoricalLibrary_133 = locationWithUnrevealed
  "03133"
  "Historical Society"
  [SecondFloor]
  NoSymbol
  [Circle]
  ("Historical Society" <:> "Historical Library")
  [SecondFloor, Passageway]
  Triangle
  [Circle, Squiggle]
  EchoesOfThePast

historicalSocietyReadingRoom :: CardDef 'LocationType
historicalSocietyReadingRoom = locationWithUnrevealed
  "03134"
  "Historical Society"
  [SecondFloor]
  NoSymbol
  [Circle]
  ("Historical Society" <:> "Reading Room")
  [SecondFloor]
  T
  [Circle]
  EchoesOfThePast

quietHalls_135 :: CardDef 'LocationType
quietHalls_135 =
  location "03135" "Quiet Halls" [ThirdFloor] Star [Circle] EchoesOfThePast

historicalSocietyHistoricalLibrary_136 :: CardDef 'LocationType
historicalSocietyHistoricalLibrary_136 = locationWithUnrevealed
  "03136"
  "Historical Society"
  [ThirdFloor]
  NoSymbol
  [Star]
  ("Historical Society" <:> "Historical Library")
  [ThirdFloor, Passageway]
  Squiggle
  [Star, Triangle]
  EchoesOfThePast

historicalSocietyPeabodysOffice :: CardDef 'LocationType
historicalSocietyPeabodysOffice = locationWithUnrevealed
  "03137"
  "Historical Society"
  [ThirdFloor]
  NoSymbol
  [Star]
  ("Historical Society" <:> "Peabody's Office")
  [ThirdFloor, Passageway]
  Moon
  [Star]
  EchoesOfThePast

historicalSocietyRecordOffice_138 :: CardDef 'LocationType
historicalSocietyRecordOffice_138 = locationWithUnrevealed
  "03138"
  "Historical Society"
  [ThirdFloor]
  NoSymbol
  [Star]
  ("Historical Society" <:> "Record Office")
  [ThirdFloor]
  Equals
  [Star]
  EchoesOfThePast

hiddenLibrary :: CardDef 'LocationType
hiddenLibrary = victory 2
  $ location "03139" "Hidden Library" mempty NoSymbol [] EchoesOfThePast

asylumHallsWesternPatientWing_168 :: CardDef 'LocationType
asylumHallsWesternPatientWing_168 = location
  "03168"
  ("Asylum Halls" <:> "Western Patient Wing")
  [ArkhamAsylum]
  Circle
  [Hourglass, Triangle, Diamond]
  TheUnspeakableOath

asylumHallsWesternPatientWing_169 :: CardDef 'LocationType
asylumHallsWesternPatientWing_169 = location
  "03169"
  ("Asylum Halls" <:> "Western Patient Wing")
  [ArkhamAsylum]
  Circle
  [Hourglass, Triangle, Diamond]
  TheUnspeakableOath

asylumHallsEasternPatientWing_170 :: CardDef 'LocationType
asylumHallsEasternPatientWing_170 = location
  "03170"
  ("Asylum Halls" <:> "Eastern Patient Wing")
  [ArkhamAsylum]
  Hourglass
  [Circle, Heart, Squiggle]
  TheUnspeakableOath

asylumHallsEasternPatientWing_171 :: CardDef 'LocationType
asylumHallsEasternPatientWing_171 = location
  "03171"
  ("Asylum Halls" <:> "Eastern Patient Wing")
  [ArkhamAsylum]
  Hourglass
  [Circle, Heart, Squiggle]
  TheUnspeakableOath

kitchen :: CardDef 'LocationType
kitchen =
  location "03172" "Kitchen" [ArkhamAsylum] Square [Triangle] TheUnspeakableOath

messHall :: CardDef 'LocationType
messHall = victory 1 $ location
  "03173"
  "Mess Hall"
  [ArkhamAsylum]
  Triangle
  [Circle, Square]
  TheUnspeakableOath

infirmary :: CardDef 'LocationType
infirmary = victory 1 $ location
  "03174"
  "Infirmary"
  [ArkhamAsylum]
  Heart
  [Hourglass]
  TheUnspeakableOath

yard :: CardDef 'LocationType
yard = location
  "03175"
  "Yard"
  [ArkhamAsylum]
  Diamond
  [Circle, Plus]
  TheUnspeakableOath

garden :: CardDef 'LocationType
garden =
  location "03176" "Garden" [ArkhamAsylum] Plus [Diamond] TheUnspeakableOath

basementHall :: CardDef 'LocationType
basementHall = victory 1 $ location
  "03177"
  "Basement Hall"
  [ArkhamAsylum]
  Squiggle
  [Hourglass, Moon]
  TheUnspeakableOath

patientConfinementDanielsCell :: CardDef 'LocationType
patientConfinementDanielsCell = locationWithUnrevealed
  "03178"
  "Patient Confinement"
  mempty
  Moon
  [Squiggle]
  ("Patient Confinement" <:> "Daniel's Cell")
  mempty
  Moon
  [Squiggle]
  TheUnspeakableOath

patientConfinementOccupiedCell :: CardDef 'LocationType
patientConfinementOccupiedCell = locationWithUnrevealed
  "03179"
  "Patient Confinement"
  mempty
  Moon
  [Squiggle]
  ("Patient Confinement" <:> "Occupied Cell")
  mempty
  Moon
  [Squiggle]
  TheUnspeakableOath

patientConfinementDrearyCell :: CardDef 'LocationType
patientConfinementDrearyCell = locationWithUnrevealed
  "03180"
  "Patient Confinement"
  mempty
  Moon
  [Squiggle]
  ("Patient Confinement" <:> "Dreary Cell")
  mempty
  Moon
  [Squiggle]
  TheUnspeakableOath

patientConfinementFamiliarCell :: CardDef 'LocationType
patientConfinementFamiliarCell = locationWithUnrevealed
  "03181"
  "Patient Confinement"
  mempty
  Moon
  [Squiggle]
  ("Patient Confinement" <:> "Familiar Cell")
  mempty
  Moon
  [Squiggle]
  TheUnspeakableOath

montparnasse :: CardDef 'LocationType
montparnasse = location
  "03208"
  "Montparnasse"
  [Paris, Rail]
  Circle
  [Heart, Star, Plus]
  APhantomOfTruth

montmartre209 :: CardDef 'LocationType
montmartre209 = location
  "03209"
  "Montmartre"
  [Paris, Rail]
  Square
  [Diamond, Triangle, Equals, Moon]
  APhantomOfTruth

montmartre210 :: CardDef 'LocationType
montmartre210 = location
  "03210"
  "Montmartre"
  [Paris, Rail]
  Square
  [Diamond, Triangle, Equals, Moon]
  APhantomOfTruth

grandGuignol :: CardDef 'LocationType
grandGuignol = victory 1 $ location
  "03211"
  ("Grand Guignol" <:> "Theatre of the Great Puppet")
  [Paris]
  Triangle
  [Diamond, Square]
  APhantomOfTruth

operaGarnier212 :: CardDef 'LocationType
operaGarnier212 = location
  "03212"
  "Opéra Garnier"
  [Paris, Rail]
  Diamond
  [Triangle, Square, Heart]
  APhantomOfTruth

operaGarnier213 :: CardDef 'LocationType
operaGarnier213 = location
  "03213"
  "Opéra Garnier"
  [Paris, Rail]
  Diamond
  [Triangle, Square, Heart]
  APhantomOfTruth

gareDOrsay :: CardDef 'LocationType
gareDOrsay = location
  "03214"
  "Gare d'Orsay"
  [Paris, Rail]
  Heart
  [Diamond, Circle, Star]
  APhantomOfTruth

pereLachaiseCemetery :: CardDef 'LocationType
pereLachaiseCemetery = victory 1 $ location
  "03215"
  "Père Lachaise Cemetery"
  [Paris]
  T
  [Equals, Moon]
  APhantomOfTruth

canalSaintMartin :: CardDef 'LocationType
canalSaintMartin = victory 1 $ location
  "03216"
  "Canal Saint-Martin"
  [Paris]
  Equals
  [Square, T, Moon]
  APhantomOfTruth

leMarais217 :: CardDef 'LocationType
leMarais217 = location
  "03217"
  "Le Marais"
  [Paris, Rail]
  Moon
  [Square, Equals, T, Plus]
  APhantomOfTruth

leMarais218 :: CardDef 'LocationType
leMarais218 = location
  "03218"
  "Le Marais"
  [Paris, Rail]
  Moon
  [Square, Equals, T, Plus]
  APhantomOfTruth

notreDame :: CardDef 'LocationType
notreDame = location
  "03219"
  "Notre-Dame"
  [Paris, Rail]
  Plus
  [Circle, Moon, Star]
  APhantomOfTruth

gardensOfLuxembourg :: CardDef 'LocationType
gardensOfLuxembourg = victory 1 $ location
  "03220"
  "Gardens of Luxembourg"
  [Paris]
  Star
  [Circle, Heart, Plus]
  APhantomOfTruth

theGateToHell :: CardDef 'LocationType
theGateToHell = locationWithUnrevealed
  "03247"
  "Catacombs"
  []
  NoSymbol
  []
  "The Gate to Hell"
  []
  NoSymbol
  []
  ThePallidMask

stoneArchways :: CardDef 'LocationType
stoneArchways = quantity 2 $ locationWithUnrevealed
  "03248"
  "Catacombs"
  []
  NoSymbol
  []
  "Stone Archways"
  []
  NoSymbol
  []
  ThePallidMask

cryptOfTheSepulchralLamp :: CardDef 'LocationType
cryptOfTheSepulchralLamp = locationWithUnrevealed
  "03249"
  "Catacombs"
  []
  NoSymbol
  []
  "Crypt of the Sepulchral Lamp"
  []
  NoSymbol
  []
  ThePallidMask

boneFilledCaverns :: CardDef 'LocationType
boneFilledCaverns = victory 1 $ locationWithUnrevealed
  "03250"
  "Catacombs"
  []
  NoSymbol
  []
  "Bone-Filled Caverns"
  []
  NoSymbol
  []
  ThePallidMask

wellOfSouls :: CardDef 'LocationType
wellOfSouls = victory 1 $ locationWithUnrevealed
  "03251"
  "Catacombs"
  []
  NoSymbol
  []
  "Well of Souls"
  []
  NoSymbol
  []
  ThePallidMask

candlelitTunnels :: CardDef 'LocationType
candlelitTunnels = quantity 2 $ locationWithUnrevealed
  "03252"
  "Catacombs"
  []
  NoSymbol
  []
  "Candlelit Tunnels"
  []
  NoSymbol
  []
  ThePallidMask

labyrinthOfBones :: CardDef 'LocationType
labyrinthOfBones = quantity 2 $ locationWithUnrevealed
  "03253"
  "Catacombs"
  []
  NoSymbol
  []
  "Labyrinth of Bones"
  []
  NoSymbol
  []
  ThePallidMask

narrowShaft :: CardDef 'LocationType
narrowShaft = victory 1 $ locationWithUnrevealed
  "03254"
  "Catacombs"
  []
  NoSymbol
  []
  "Narrow Shaft"
  []
  NoSymbol
  []
  ThePallidMask

shiveringPools :: CardDef 'LocationType
shiveringPools = victory 1 $ locationWithUnrevealed
  "03255"
  "Catacombs"
  []
  NoSymbol
  []
  "Shivering Pools"
  []
  NoSymbol
  []
  ThePallidMask

blockedPassage :: CardDef 'LocationType
blockedPassage = locationWithUnrevealed
  "03256"
  "Catacombs"
  []
  NoSymbol
  []
  "Blocked Passage"
  []
  NoSymbol
  []
  ThePallidMask

tombOfShadows :: CardDef 'LocationType
tombOfShadows = victory 1 $ locationWithUnrevealed
  "03257"
  "Catacombs"
  []
  NoSymbol
  []
  "Tomb of Shadows"
  []
  NoSymbol
  []
  ThePallidMask

porteDeLAvancee :: CardDef 'LocationType
porteDeLAvancee =
  location "03283" "Porte de l’Avancée" [] Circle [Squiggle] BlackStarsRise

grandRue :: CardDef 'LocationType
grandRue = location
  "03284"
  "Grand Rue"
  []
  Squiggle
  [Circle, Triangle, Diamond, Equals]
  BlackStarsRise

outerWall_285 :: CardDef 'LocationType
outerWall_285 = victory 1 $ location
  "03285"
  "Outer Wall"
  []
  Triangle
  [Squiggle, Diamond, Equals]
  BlackStarsRise

outerWall_286 :: CardDef 'LocationType
outerWall_286 = victory 1 $ location
  "03286"
  "Outer Wall"
  []
  Triangle
  [Squiggle, Diamond, Equals]
  BlackStarsRise

northTower_287 :: CardDef 'LocationType
northTower_287 = victory 1 $ location
  "03287"
  "North Tower"
  []
  Diamond
  [Squiggle, Triangle, Equals]
  BlackStarsRise

northTower_288 :: CardDef 'LocationType
northTower_288 = victory 1 $ location
  "03288"
  "North Tower"
  []
  Diamond
  [Squiggle, Triangle, Equals]
  BlackStarsRise

brokenSteps_289 :: CardDef 'LocationType
brokenSteps_289 = location
  "03289"
  "Broken Steps"
  []
  Equals
  [Squiggle, Triangle, Diamond, Square]
  BlackStarsRise

brokenSteps_290 :: CardDef 'LocationType
brokenSteps_290 = location
  "03290"
  "Broken Steps"
  []
  Equals
  [Squiggle, Triangle, Diamond, Square]
  BlackStarsRise

abbeyChurch :: CardDef 'LocationType
abbeyChurch = location
  "03291"
  "Abbey Church"
  []
  Square
  [Equals, T, Heart, Hourglass, Moon]
  BlackStarsRise

choeurGothique_292 :: CardDef 'LocationType
choeurGothique_292 =
  location "03292" "Chœur Gothique" [] T [Square, Star] BlackStarsRise

choeurGothique_293 :: CardDef 'LocationType
choeurGothique_293 =
  location "03293" "Chœur Gothique" [] T [Square, Star] BlackStarsRise

cloister :: CardDef 'LocationType
cloister =
  location "03294" "Cloister" [] Heart [Square, Hourglass] BlackStarsRise

knightsHall :: CardDef 'LocationType
knightsHall =
  location "03295" "Knight's Hall" [] Hourglass [Square, Heart] BlackStarsRise

chapelOfStAubertThePathIsOpen :: CardDef 'LocationType
chapelOfStAubertThePathIsOpen = locationWithUnrevealed
  "03296"
  "Chapel of St. Aubert"
  []
  Moon
  [Square]
  ("Chapel of St. Aubert" <:> "The Path is Open")
  []
  Moon
  [Square]
  BlackStarsRise

chapelOfStAubertWatersForbidden :: CardDef 'LocationType
chapelOfStAubertWatersForbidden = victory 2 $ locationWithUnrevealed
  "03297"
  "Chapel of St. Aubert"
  []
  Moon
  [Square]
  ("Chapel of St. Aubert" <:> "Waters Forbidden")
  []
  Moon
  [Square]
  BlackStarsRise

abbeyTowerThePathIsOpen :: CardDef 'LocationType
abbeyTowerThePathIsOpen = locationWithUnrevealed
  "03298"
  "Abbey Tower"
  []
  Star
  [T]
  ("Abbey Tower" <:> "The Path is Open")
  []
  Star
  [T]
  BlackStarsRise

abbeyTowerSpiresForbidden :: CardDef 'LocationType
abbeyTowerSpiresForbidden = victory 2 $ locationWithUnrevealed
  "03299"
  "Abbey Tower"
  []
  Star
  [T]
  ("Abbey Tower" <:> "Spires Forbidden")
  []
  Star
  [T]
  BlackStarsRise

shoresOfHali :: CardDef 'LocationType
shoresOfHali =
  location "03325b" "Shores of Hali" [Otherworld] Circle [Square] DimCarcosa

bleakPlainsStarsOfAldebaran :: CardDef 'LocationType
bleakPlainsStarsOfAldebaran = location
  "03326b"
  "Bleak Plains"
  [Otherworld]
  Square
  [Circle, Triangle, Diamond]
  DimCarcosa

bleakPlainsBleakDesolation :: CardDef 'LocationType
bleakPlainsBleakDesolation = location
  "03326d"
  "Bleak Plains"
  [Otherworld]
  Square
  [Circle, Triangle, Diamond]
  DimCarcosa

ruinsOfCarcosaInhabitantOfCarcosa :: CardDef 'LocationType
ruinsOfCarcosaInhabitantOfCarcosa = location
  "03327b"
  "Ruins of Carcosa"
  [Otherworld]
  Triangle
  [Square, Equals, Star]
  DimCarcosa

ruinsOfCarcosaAMomentsRest :: CardDef 'LocationType
ruinsOfCarcosaAMomentsRest = location
  "03327d"
  "Ruins of Carcosa"
  [Otherworld]
  Triangle
  [Square, Equals, Star]
  DimCarcosa


ruinsOfCarcosaTheCoffin :: CardDef 'LocationType
ruinsOfCarcosaTheCoffin = location
  "03327f"
  "Ruins of Carcosa"
  [Otherworld]
  Triangle
  [Square, Equals, Star]
  DimCarcosa

dimStreetsMappingTheStreets :: CardDef 'LocationType
dimStreetsMappingTheStreets = location
  "03328b"
  "Dim Streets"
  [Otherworld]
  Diamond
  [Square, Equals, Star]
  DimCarcosa

dimStreetsTheKingsParade :: CardDef 'LocationType
dimStreetsTheKingsParade = location
  "03328d"
  "Dim Streets"
  [Otherworld]
  Diamond
  [Square, Equals, Star]
  DimCarcosa

dimStreetsTheArchway :: CardDef 'LocationType
dimStreetsTheArchway = location
  "03328f"
  "Dim Streets"
  [Otherworld]
  Diamond
  [Square, Equals, Star]
  DimCarcosa

depthsOfDemheTheHeightOfTheDepths :: CardDef 'LocationType
depthsOfDemheTheHeightOfTheDepths = location
  "03329b"
  "Depths of Demhe"
  [Otherworld]
  Equals
  [Moon, Triangle, Diamond]
  DimCarcosa

depthsOfDemheStepsOfThePalace :: CardDef 'LocationType
depthsOfDemheStepsOfThePalace = location
  "03329d"
  "Depths of Demhe"
  [Otherworld]
  Equals
  [Moon, Triangle, Diamond]
  DimCarcosa

darkSpires :: CardDef 'LocationType
darkSpires =
  location "03330b" "Dark Spires" [Otherworld] Moon [Equals] DimCarcosa

palaceOfTheKing :: CardDef 'LocationType
palaceOfTheKing = location
  "03331b"
  "Palace of the King"
  [Otherworld]
  Star
  [Triangle, Diamond]
  DimCarcosa

expeditionCamp :: CardDef 'LocationType
expeditionCamp = location
  "04050"
  "Expedition Camp"
  [Campsite, Jungle]
  Circle
  [Square, Diamond, Moon]
  TheUntamedWilds

ruinsOfEztli :: CardDef 'LocationType
ruinsOfEztli = victory 2 $ singleSided $ location
  "04053"
  "Ruins of Eztli"
  [Ancient, Ruins]
  Hourglass
  [Triangle, Heart]
  TheUntamedWilds

entryway :: CardDef 'LocationType
entryway = location
  "04060"
  "Entryway"
  [Ancient, Ruins]
  Circle
  [Square, Star]
  TheDoomOfEztli

ancientHall :: CardDef 'LocationType
ancientHall = singleSided $ location
  "04063"
  "Ancient Hall"
  [Ancient, Ruins]
  Square
  [Circle, Star, Diamond]
  TheDoomOfEztli

grandChamber :: CardDef 'LocationType
grandChamber = victory 1 $ singleSided $ location
  "04064"
  "Grand Chamber"
  [Ancient, Ruins]
  Star
  [Circle, Square, Triangle]
  TheDoomOfEztli

burialPit :: CardDef 'LocationType
burialPit = victory 1 $ singleSided $ location
  "04065"
  "Burial Pit"
  [Ancient, Ruins]
  Triangle
  [Star, Diamond, Squiggle]
  TheDoomOfEztli

undergroundRuins :: CardDef 'LocationType
undergroundRuins = vengeance 1 $ singleSided $ location
  "04066"
  "Underground Ruins"
  [Ancient, Ruins]
  Diamond
  [Square, Triangle, Squiggle]
  TheDoomOfEztli

secretPassage :: CardDef 'LocationType
secretPassage = victory 1 $ singleSided $ location
  "04067"
  "Secret Passage"
  [Ancient, Ruins]
  Squiggle
  [Diamond, Triangle, Hourglass]
  TheDoomOfEztli

chamberOfTime :: CardDef 'LocationType
chamberOfTime = victory 2 $ vengeance 2 $ singleSided $ location
  "04068"
  "Chamber of Time"
  [Forgotten, Ruins]
  Hourglass
  [Squiggle]
  TheDoomOfEztli

pathOfThorns :: CardDef 'LocationType
pathOfThorns = singleSided $ location
  "04069"
  "Path of Thorns"
  [Jungle]
  Square
  [Circle, Diamond, Triangle, Squiggle]
  Rainforest

riverCanyon :: CardDef 'LocationType
riverCanyon = singleSided $ location
  "04070"
  "River Canyon"
  [Jungle]
  Diamond
  [Circle, Moon, Heart, Triangle, Square]
  Rainforest

ropeBridge :: CardDef 'LocationType
ropeBridge = singleSided $ location
  "04071"
  "Rope Bridge"
  [Jungle]
  Moon
  [Circle, Diamond, Heart, T]
  Rainforest

serpentsHaven :: CardDef 'LocationType
serpentsHaven = victory 1 $ singleSided $ location
  "04072"
  "Serpent's Haven"
  [Jungle]
  Triangle
  [Squiggle, Square, Diamond, Hourglass]
  Rainforest

circuitousTrail :: CardDef 'LocationType
circuitousTrail = victory 1 $ singleSided $ location
  "04073"
  "Circuitous Trail"
  [Jungle]
  Heart
  [Hourglass, Diamond, Moon, T]
  Rainforest

templeOfTheFang :: CardDef 'LocationType
templeOfTheFang = victory 2 $ singleSided $ location
  "04074"
  "Temple of the Fang"
  [Ancient, Ruins]
  Squiggle
  [Square, Triangle, Equals]
  Rainforest

overgrownRuins :: CardDef 'LocationType
overgrownRuins = victory 2 $ singleSided $ location
  "04075"
  "Overgrown Ruins"
  [Ancient, Ruins]
  T
  [Moon, Heart, Equals]
  Rainforest

eztliExhibit :: CardDef 'LocationType
eztliExhibit = victory 1 $ singleSided $ location
  "04117b"
  "Eztli Exhibit"
  [Miskatonic, Exhibit]
  Plus
  [Diamond]
  ThreadsOfFate

velmasDiner :: CardDef 'LocationType
velmasDiner =
  location "04141" "Velma's Diner" [Arkham] NoSymbol [Moon] ThreadsOfFate

curiositieShoppe :: CardDef 'LocationType
curiositieShoppe = victory 1
  $ location "04142" "Curiositie Shoppe" [Arkham] NoSymbol [T] ThreadsOfFate

townHall :: CardDef 'LocationType
townHall = victory 1
  $ location "04143" "Town Hall" [Arkham] NoSymbol [Triangle] ThreadsOfFate

arkhamPoliceStation :: CardDef 'LocationType
arkhamPoliceStation = victory 1 $ singleSided $ location
  "04126b"
  "Arkham Police Station"
  [Arkham]
  NoSymbol
  [Moon]
  ThreadsOfFate

trainTracks :: CardDef 'LocationType
trainTracks = singleSided
  $ location "04128b" "Train Tracks" [Arkham] NoSymbol [T] ThreadsOfFate

blackCave :: CardDef 'LocationType
blackCave = victory 1 $ singleSided $ location
  "04133f"
  "Black Cave"
  [Cave]
  Hourglass
  [Circle]
  ThreadsOfFate

templeRuins :: CardDef 'LocationType
templeRuins = location
  "04168"
  "Temple Ruins"
  [MexicoCity, PresentDay]
  Circle
  [Diamond, Star]
  TheBoundaryBeyond

metropolitanCathedral :: CardDef 'LocationType
metropolitanCathedral = location
  "04169"
  "Metropolitan Cathedral"
  [MexicoCity, PresentDay]
  Square
  [Diamond]
  TheBoundaryBeyond

chapultepecPark :: CardDef 'LocationType
chapultepecPark = location
  "04170"
  "Chapultepec Park"
  [MexicoCity, PresentDay]
  Triangle
  [Star]
  TheBoundaryBeyond

zocalo :: CardDef 'LocationType
zocalo = location
  "04171"
  "Zócalo"
  [MexicoCity, PresentDay]
  Diamond
  [Heart, Square, Star, Circle]
  TheBoundaryBeyond

xochimilco :: CardDef 'LocationType
xochimilco = location
  "04172"
  "Xochimilco"
  [MexicoCity, PresentDay]
  Heart
  [Diamond, Star]
  TheBoundaryBeyond

coyoacan :: CardDef 'LocationType
coyoacan = location
  "04173"
  "Coyoacán"
  [MexicoCity, PresentDay]
  Star
  [Diamond, Triangle, Circle, Heart]
  TheBoundaryBeyond

temploMayor_174 :: CardDef 'LocationType
temploMayor_174 = singleSided $ location
  "04174"
  "Templo Mayor"
  [Ancient, Tenochtitlan]
  Circle
  [Square, Triangle]
  TheBoundaryBeyond

temploMayor_175 :: CardDef 'LocationType
temploMayor_175 = singleSided $ location
  "04175"
  "Templo Mayor"
  [Ancient, Tenochtitlan]
  Circle
  [Square, Triangle]
  TheBoundaryBeyond

templesOfTenochtitlan_176 :: CardDef 'LocationType
templesOfTenochtitlan_176 = singleSided $ location
  "04176"
  "Temples of Tenochtitlán"
  [Ancient, Tenochtitlan]
  Square
  [Diamond, Circle]
  TheBoundaryBeyond

templesOfTenochtitlan_177 :: CardDef 'LocationType
templesOfTenochtitlan_177 = singleSided $ location
  "04177"
  "Temples of Tenochtitlán"
  [Ancient, Tenochtitlan]
  Square
  [Diamond, Circle]
  TheBoundaryBeyond

chapultepecHill_178 :: CardDef 'LocationType
chapultepecHill_178 = singleSided $ location
  "04178"
  "Chapultepec Hill"
  [Ancient, Tenochtitlan]
  Triangle
  [Star, Circle]
  TheBoundaryBeyond

chapultepecHill_179 :: CardDef 'LocationType
chapultepecHill_179 = singleSided $ location
  "04179"
  "Chapultepec Hill"
  [Ancient, Tenochtitlan]
  Triangle
  [Star, Circle]
  TheBoundaryBeyond

canalsOfTenochtitlan_180 :: CardDef 'LocationType
canalsOfTenochtitlan_180 = singleSided $ location
  "04180"
  "Canals of Tenochtitlán"
  [Ancient, Tenochtitlan]
  Diamond
  [Heart, Square]
  TheBoundaryBeyond

canalsOfTenochtitlan_181 :: CardDef 'LocationType
canalsOfTenochtitlan_181 = singleSided $ location
  "04181"
  "Canals of Tenochtitlán"
  [Ancient, Tenochtitlan]
  Diamond
  [Heart, Square]
  TheBoundaryBeyond

lakeXochimilco_182 :: CardDef 'LocationType
lakeXochimilco_182 = singleSided $ location
  "04182"
  "Lake Xochimilco"
  [Ancient, Tenochtitlan]
  Heart
  [Diamond, Star]
  TheBoundaryBeyond

lakeXochimilco_183 :: CardDef 'LocationType
lakeXochimilco_183 = singleSided $ location
  "04183"
  "Lake Xochimilco"
  [Ancient, Tenochtitlan]
  Heart
  [Diamond, Star]
  TheBoundaryBeyond

sacredWoods_184 :: CardDef 'LocationType
sacredWoods_184 = singleSided $ location
  "04184"
  "Sacred Woods"
  [Ancient, Tenochtitlan]
  Star
  [Heart, Triangle]
  TheBoundaryBeyond

sacredWoods_185 :: CardDef 'LocationType
sacredWoods_185 = singleSided $ location
  "04185"
  "Sacred Woods"
  [Ancient, Tenochtitlan]
  Star
  [Heart, Triangle]
  TheBoundaryBeyond

mouthOfKnYanTheCavernsMaw :: CardDef 'LocationType
mouthOfKnYanTheCavernsMaw = singleSided $ location
  "04206"
  ("Mouth of K'n-yan" <:> "The Cavern's Maw")
  [Cave]
  Equals
  [Squiggle, T, Hourglass]
  HeartOfTheElders

mouthOfKnYanTheDepthsBelow :: CardDef 'LocationType
mouthOfKnYanTheDepthsBelow = singleSided $ location
  "04206b"
  ("Mouth of K'n-yan" <:> "The Depths Below")
  [Cave]
  Equals
  [Circle, Triangle, Diamond]
  HeartOfTheElders

timeWrackedWoods :: CardDef 'LocationType
timeWrackedWoods = singleSided $ location
  "04217"
  "Time-Wracked Woods"
  [Jungle]
  Circle
  [Square, Diamond, Moon]
  PillarsOfJudgement

stoneAltar :: CardDef 'LocationType
stoneAltar = victory 1 $ singleSided $ location
  "04218"
  "Stone Altar"
  [Ancient, Ruins]
  Hourglass
  [Triangle, Heart, Equals]
  PillarsOfJudgement

vastPassages :: CardDef 'LocationType
vastPassages = singleSided $ location
  "04222"
  "Vast Passages"
  [Ancient, Cave]
  Circle
  [Equals, Triangle, Diamond, Square, Moon]
  KnYan

hallOfIdolatry :: CardDef 'LocationType
hallOfIdolatry = victory 1 $ singleSided $ location
  "04223"
  "Hall of Idolatry"
  [Ancient, Cave]
  Square
  [Heart, Triangle, Circle]
  KnYan

darkHollow :: CardDef 'LocationType
darkHollow = victory 1 $ singleSided $ location
  "04224"
  "Dark Hollow"
  [Ancient, Cave]
  Triangle
  [Equals, Circle, Square]
  KnYan

perilousGulch :: CardDef 'LocationType
perilousGulch = victory 1 $ singleSided $ location
  "04225"
  "Perilous Gulch"
  [Ancient, Cave]
  Diamond
  [Equals, Circle, Moon]
  KnYan

crystalPillars :: CardDef 'LocationType
crystalPillars = victory 1 $ singleSided $ location
  "04226"
  "Crystal Pillars"
  [Ancient, Cave]
  Moon
  [Heart, Diamond, Circle]
  KnYan

descentToYoth :: CardDef 'LocationType
descentToYoth = victory 2 $ vengeance 2 $ singleSided $ location
  "04227"
  "Descent to Yoth"
  [Ancient, Cave]
  Heart
  [Square, Moon]
  KnYan

interviewRoomArrivalChamber :: CardDef 'LocationType
interviewRoomArrivalChamber = locationWithUnrevealed
  "04245"
  "Interview Room"
  [Ancient, Pnakotus]
  Droplet
  [Diamond]
  ("Interview Room" <:> "Arrival Chamber")
  [Ancient, Pnakotus]
  Droplet
  [Diamond]
  TheCityOfArchives

interviewRoomRestrainingChamber :: CardDef 'LocationType
interviewRoomRestrainingChamber = locationWithUnrevealed
  "04246"
  "Interview Room"
  [Ancient, Pnakotus]
  Droplet
  [Diamond]
  ("Interview Room" <:> "Restraining Chamber")
  [Ancient, Pnakotus]
  Droplet
  [Diamond]
  TheCityOfArchives

interviewRoomIchorFilledChamber :: CardDef 'LocationType
interviewRoomIchorFilledChamber = locationWithUnrevealed
  "04247"
  "Interview Room"
  [Ancient, Pnakotus]
  Droplet
  [Diamond]
  ("Interview Room" <:> "Ichor-Filled Chamber")
  [Ancient, Pnakotus]
  Droplet
  [Diamond]
  TheCityOfArchives

hallsOfPnakotusNorthernCorridors :: CardDef 'LocationType
hallsOfPnakotusNorthernCorridors = location
  "04248"
  ("Halls of Pnakotus" <:> "Northern Corridors")
  [Ancient, Pnakotus]
  Squiggle
  [Diamond, Square, Triangle]
  TheCityOfArchives

hallsOfPnakotusEasternCorridors :: CardDef 'LocationType
hallsOfPnakotusEasternCorridors = location
  "04249"
  ("Halls of Pnakotus" <:> "Eastern Corridors")
  [Ancient, Pnakotus]
  Diamond
  [Squiggle, Square, Droplet]
  TheCityOfArchives

hallsOfPnakotusWesternCorridors :: CardDef 'LocationType
hallsOfPnakotusWesternCorridors = location
  "04250"
  ("Halls of Pnakotus" <:> "Western Corridors")
  [Ancient, Pnakotus]
  Square
  [Squiggle, Diamond, Circle, Star]
  TheCityOfArchives

greatLibrary :: CardDef 'LocationType
greatLibrary = location
  "04251"
  "Great Library"
  [Ancient, Pnakotus]
  Circle
  [Square]
  TheCityOfArchives

yithianOrrery :: CardDef 'LocationType
yithianOrrery = victory 1 $ location
  "04252"
  "Yithian Orrery"
  [Ancient, Pnakotus]
  Moon
  [Triangle]
  TheCityOfArchives

laboratoryOfTheGreatRace :: CardDef 'LocationType
laboratoryOfTheGreatRace = location
  "04253"
  "Laboratory of the Great Race"
  [Ancient, Pnakotus]
  Triangle
  [Squiggle, Moon, Equals]
  TheCityOfArchives

deconstructionRoom :: CardDef 'LocationType
deconstructionRoom = victory 1 $ location
  "04254"
  "Deconstruction Room"
  [Ancient, Pnakotus]
  Equals
  [Triangle]
  TheCityOfArchives

towersOfPnakotus :: CardDef 'LocationType
towersOfPnakotus = victory 2 $ location
  "04255"
  "Towers of Pnakotus"
  [Ancient, Pnakotus]
  Star
  [Square]
  TheCityOfArchives

stepsOfYoth :: CardDef 'LocationType
stepsOfYoth = singleSided $ location
  "04286"
  "Steps of Yoth"
  [Ancient, Forgotten, Yoth]
  Equals
  [Hourglass, Square, Triangle, Diamond, Heart]
  TheDepthsOfYoth

cityOfTheSerpents :: CardDef 'LocationType
cityOfTheSerpents = vengeance 2 $ singleSided $ location
  "04287"
  "City of the Serpents"
  [Ancient, Cave, Yoth]
  Diamond
  [Equals, Droplet, Triangle, T, Square]
  TheDepthsOfYoth

hallOfHeresy :: CardDef 'LocationType
hallOfHeresy = vengeance 2 $ singleSided $ location
  "04288"
  "Hall of Heresy"
  [Ancient, Cave, Yoth]
  Triangle
  [Equals, Diamond, Circle, Square, T]
  TheDepthsOfYoth

crumblingPrecipice :: CardDef 'LocationType
crumblingPrecipice = singleSided $ location
  "04289"
  "Crumbling Precipice"
  [Ancient, Cave, Yoth]
  Hourglass
  [Equals, Squiggle, Heart, T, Droplet]
  TheDepthsOfYoth

cavernsOfYoth :: CardDef 'LocationType
cavernsOfYoth = singleSided $ location
  "04290"
  "Caverns of Yoth"
  [Ancient, Cave, Yoth]
  Droplet
  [Circle, Hourglass, Heart, Diamond, Squiggle]
  TheDepthsOfYoth

forkedPath :: CardDef 'LocationType
forkedPath = singleSided $ location
  "04291"
  "Forked Path"
  [Ancient, Cave, Yoth]
  T
  [Circle, Diamond, Hourglass, Square, Triangle]
  TheDepthsOfYoth

bridgeOverNKai :: CardDef 'LocationType
bridgeOverNKai = singleSided $ location
  "04292"
  "Bridge ver N'Kai"
  [Ancient, Cave, Yoth]
  Heart
  [Equals, Circle, Droplet, Hourglass, Squiggle]
  TheDepthsOfYoth

brokenPassage :: CardDef 'LocationType
brokenPassage = singleSided $ location
  "04293"
  "Broken Passage"
  [Ancient, Cave, Yoth]
  Squiggle
  [Circle, Droplet, Hourglass, Square, Heart]
  TheDepthsOfYoth

abandonedSite :: CardDef 'LocationType
abandonedSite = singleSided $ location
  "04294"
  "Abandoned Site"
  [Ancient, Cave, Yoth]
  Square
  [Equals, Diamond, Triangle, T, Squiggle]
  TheDepthsOfYoth

brightCanyon :: CardDef 'LocationType
brightCanyon = singleSided $ location
  "04295"
  "Bright Canyon"
  [Ancient, Cave, Yoth]
  Circle
  [Droplet, Squiggle, T, Heart, Triangle]
  TheDepthsOfYoth

nexusOfNKai :: CardDef 'LocationType
nexusOfNKai = location
  "04324"
  ("Nexus of N'kai" <:> "Unraveling the Threads")
  [Ancient, Ruins]
  Diamond
  [Droplet, Star]
  ShatteredAeons

yuggoth :: CardDef 'LocationType
yuggoth = singleSided
  $ location "04327" "Yuggoth" [Otherworld] Droplet [Diamond] ShatteredAeons

shoresOfRlyeh :: CardDef 'LocationType
shoresOfRlyeh = singleSided $ location
  "04328"
  "Shores of R'lyeh"
  [Otherworld]
  Droplet
  [Diamond]
  ShatteredAeons

cityOfTheUnseen :: CardDef 'LocationType
cityOfTheUnseen = singleSided $ location
  "04329"
  "City of the Unseen"
  [Otherworld]
  Droplet
  [Diamond]
  ShatteredAeons

aPocketInTime :: CardDef 'LocationType
aPocketInTime = victory 1 $ singleSided $ location
  "04330"
  "A Pocket in Time"
  [Extradimensional]
  Star
  [Diamond, Equals]
  ShatteredAeons

ruinsOfNewYork :: CardDef 'LocationType
ruinsOfNewYork = singleSided $ location
  "04331"
  "Ruins of New York"
  [Shattered, Future, Ruins]
  Equals
  [Star]
  ShatteredAeons

mu :: CardDef 'LocationType
mu = victory 1 $ singleSided $ location
  "04332"
  "Mu"
  [Shattered, Ancient]
  Equals
  [Star]
  ShatteredAeons

atlantis :: CardDef 'LocationType
atlantis = singleSided $ location
  "04333"
  "Atlantis"
  [Shattered, Ancient]
  Equals
  [Star]
  ShatteredAeons

pnakotus :: CardDef 'LocationType
pnakotus = victory 1 $ singleSided $ location
  "04334"
  "Pnakotus"
  [Shattered, Ancient]
  Equals
  [Star]
  ShatteredAeons

valusia :: CardDef 'LocationType
valusia = victory 1 $ singleSided $ location
  "04335"
  "Valusia"
  [Shattered, Ancient]
  Equals
  [Star]
  ShatteredAeons

plateauOfLeng :: CardDef 'LocationType
plateauOfLeng = singleSided $ location
  "04336"
  "Plateau of Leng"
  [Shattered, PresentDay]
  Equals
  [Star]
  ShatteredAeons

studyAberrantGateway :: CardDef 'LocationType
studyAberrantGateway = location
  "50013"
  ("Study" <:> "Aberrant Gateway")
  mempty
  Circle
  [T]
  ReturnToTheGathering

guestHall :: CardDef 'LocationType
guestHall = location
  "50014"
  "Guest Hall"
  mempty
  T
  [Circle, Heart, Star, Square]
  ReturnToTheGathering

bedroom :: CardDef 'LocationType
bedroom = location "50015" "Bedroom" mempty Heart [T] ReturnToTheGathering

bathroom :: CardDef 'LocationType
bathroom = location "50016" "Bathroom" mempty Star [T] ReturnToTheGathering

holeInTheWall :: CardDef 'LocationType
holeInTheWall = locationWithUnrevealed
  "50017"
  "Hole in the Wall"
  mempty
  Square
  [T]
  "Hallway"
  mempty
  Square
  [T, Triangle, Plus, Diamond]
  ReturnToTheGathering

returnToAttic :: CardDef 'LocationType
returnToAttic = locationWithUnrevealed
  "50018"
  "Attic"
  mempty
  Triangle
  [Square]
  "Attic"
  mempty
  Triangle
  [Square, Moon]
  ReturnToTheGathering

farAboveYourHouse :: CardDef 'LocationType
farAboveYourHouse = victory 1 $ locationWithUnrevealed
  "50019"
  "Far Above Your House"
  mempty
  Moon
  [Triangle]
  "Field of Graves"
  mempty
  Moon
  [Triangle]
  ReturnToTheGathering

returnToCellar :: CardDef 'LocationType
returnToCellar = locationWithUnrevealed
  "50020"
  "Cellar"
  mempty
  Plus
  [Square]
  "Cellar"
  mempty
  Plus
  [Square, Squiggle]
  ReturnToTheGathering

deepBelowYourHouse :: CardDef 'LocationType
deepBelowYourHouse = victory 1 $ locationWithUnrevealed
  "50021"
  "Deep Below Your House"
  mempty
  Squiggle
  [Plus]
  "Ghoul Pits"
  mempty
  Squiggle
  [Plus]
  ReturnToTheGathering

easttownArkhamPoliceStation :: CardDef 'LocationType
easttownArkhamPoliceStation = victory 1 $ locationWithUnrevealed
  "50027"
  "Easttown"
  [Arkham]
  Moon
  [Circle, Triangle]
  ("Easttown" <:> "Arkham Police Station")
  [Arkham]
  Moon
  [Circle, Triangle]
  ReturnToTheMidnightMasks

northsideTrainStation :: CardDef 'LocationType
northsideTrainStation = locationWithUnrevealed
  "50028"
  ("Northside" <:> "Train Station")
  [Arkham]
  T
  [Diamond, Triangle]
  ("Northside" <:> "Train Station")
  [Arkham]
  T
  [Diamond, Triangle]
  ReturnToTheMidnightMasks

miskatonicUniversityMiskatonicMuseum :: CardDef 'LocationType
miskatonicUniversityMiskatonicMuseum = locationWithUnrevealed
  "50029"
  "Miskatonic University"
  [Arkham]
  Diamond
  [T, Plus, Circle, Square]
  ("Miskatonic University" <:> "Miskatonic Museum")
  [Arkham]
  Diamond
  [T, Plus, Circle, Square]
  ReturnToTheMidnightMasks

rivertownAbandonedWarehouse :: CardDef 'LocationType
rivertownAbandonedWarehouse = locationWithUnrevealed
  "50030"
  "Rivertown"
  [Arkham, Central]
  Circle
  [Moon, Diamond, Square, Squiggle, Hourglass]
  ("Rivertown" <:> "Abandoned Warehouse")
  [Arkham, Central]
  Circle
  [Moon, Diamond, Square, Squiggle, Hourglass]
  ReturnToTheMidnightMasks

arkhamWoodsGreatWillow :: CardDef 'LocationType
arkhamWoodsGreatWillow = locationWithUnrevealed
  "50033"
  "Arkham Woods"
  [Woods]
  Square
  [Squiggle]
  ("Arkham Woods" <:> "Great Willow")
  [Woods]
  Heart
  [Squiggle, Star]
  ReturnToTheDevourerBelow

arkhamWoodsLakeside :: CardDef 'LocationType
arkhamWoodsLakeside = locationWithUnrevealed
  "50034"
  "Arkham Woods"
  [Woods]
  Square
  [Squiggle]
  ("Arkham Woods" <:> "Lakeside")
  [Woods]
  Star
  [Squiggle, Heart]
  ReturnToTheDevourerBelow

arkhamWoodsCorpseRiddenClearing :: CardDef 'LocationType
arkhamWoodsCorpseRiddenClearing = locationWithUnrevealed
  "50035"
  "Arkham Woods"
  [Woods]
  Square
  [Squiggle]
  ("Arkham Woods" <:> "Corpse-Ridden Clearing")
  [Woods]
  Droplet
  [Squiggle, Circle]
  ReturnToTheDevourerBelow

arkhamWoodsWoodenBridge :: CardDef 'LocationType
arkhamWoodsWoodenBridge = locationWithUnrevealed
  "50036"
  "Arkham Woods"
  [Woods]
  Square
  [Squiggle]
  ("Arkham Woods" <:> "Wooden Bridge")
  [Woods]
  Circle
  [Squiggle, Droplet]
  ReturnToTheDevourerBelow

cursedShores :: CardDef 'LocationType
cursedShores = location
  "81007"
  "Cursed Shores"
  [NewOrleans, Bayou]
  Square
  [Plus, Triangle, Diamond, Hourglass]
  TheBayou

gardenDistrict :: CardDef 'LocationType
gardenDistrict = locationWithUnrevealed
  "81008"
  "New Orleans"
  [NewOrleans]
  Plus
  [Square, Plus]
  "Garden District"
  [NewOrleans]
  Plus
  [Square, Plus]
  TheBayou

broadmoor :: CardDef 'LocationType
broadmoor = victory 1 $ locationWithUnrevealed
  "81009"
  "New Orleans"
  [NewOrleans]
  Plus
  [Square, Plus]
  "Broadmoor"
  [NewOrleans]
  Plus
  [Square, Plus]
  TheBayou

brackishWaters :: CardDef 'LocationType
brackishWaters = location
  "81010"
  "Brackish Waters"
  [Riverside, Bayou]
  Triangle
  [Squiggle, Square, Diamond, Hourglass]
  TheBayou

audubonPark :: CardDef 'LocationType
audubonPark = victory 1 $ locationWithUnrevealed
  "81011"
  "Riverside"
  [Riverside]
  Squiggle
  [Triangle, Squiggle]
  "Audubon Park"
  [Riverside]
  Squiggle
  [Triangle, Squiggle]
  TheBayou

fauborgMarigny :: CardDef 'LocationType
fauborgMarigny = locationWithUnrevealed
  "81012"
  "Riverside"
  [Riverside]
  Squiggle
  [Triangle, Squiggle]
  "Fauborg Marigny"
  [Riverside]
  Squiggle
  [Triangle, Squiggle]
  TheBayou

forgottenMarsh :: CardDef 'LocationType
forgottenMarsh = location
  "81013"
  "Forgotten Marsh"
  [Wilderness, Bayou]
  Diamond
  [Moon, Square, Triangle, Hourglass]
  TheBayou

trappersCabin :: CardDef 'LocationType
trappersCabin = locationWithUnrevealed
  "81014"
  "Wilderness"
  [Wilderness]
  Moon
  [Diamond, Moon]
  "Trapper's Cabin"
  [Wilderness]
  Moon
  [Diamond, Moon]
  TheBayou

twistedUnderbrush :: CardDef 'LocationType
twistedUnderbrush = victory 1 $ locationWithUnrevealed
  "81015"
  "Wilderness"
  [Wilderness]
  Moon
  [Diamond, Moon]
  "Twisted Underbrush"
  [Wilderness]
  Moon
  [Diamond, Moon]
  TheBayou

foulSwamp :: CardDef 'LocationType
foulSwamp = location
  "81016"
  "Foul Swamp"
  [Unhallowed, Bayou]
  Hourglass
  [Equals, Square, Triangle, Diamond]
  TheBayou

ritualGrounds :: CardDef 'LocationType
ritualGrounds = victory 1 $ locationWithUnrevealed
  "81017"
  "Unhallowed Land"
  [Unhallowed]
  Equals
  [Hourglass, Equals]
  "Ritual Grounds"
  [Unhallowed]
  Equals
  [Hourglass, Equals]
  TheBayou

overgrownCairns :: CardDef 'LocationType
overgrownCairns = locationWithUnrevealed
  "81018"
  "Unhallowed Land"
  [Unhallowed]
  Equals
  [Hourglass, Equals]
  "Overgrown Cairns"
  [Unhallowed]
  Equals
  [Hourglass, Equals]
  TheBayou

gondola :: CardDef 'LocationType
gondola =
  location "82006b" "Gondola" [Venice, Boat] NoSymbol [] CarnevaleOfHorrors

sanMarcoBasilica :: CardDef 'LocationType
sanMarcoBasilica =
  location "82008" "San Marco Basilica" [Venice] NoSymbol [] CarnevaleOfHorrors

canalSide :: CardDef 'LocationType
canalSide =
  location "82009" "Canal-side" [Venice] NoSymbol [] CarnevaleOfHorrors

streetsOfVenice :: CardDef 'LocationType
streetsOfVenice =
  location "82010" "Streets of Venice" [Venice] NoSymbol [] CarnevaleOfHorrors

rialtoBridge :: CardDef 'LocationType
rialtoBridge = location
  "82011"
  "Rialto Bridge"
  [Venice, Bridge]
  NoSymbol
  []
  CarnevaleOfHorrors

venetianGarden :: CardDef 'LocationType
venetianGarden =
  location "82012" "Venetian Garden" [Venice] NoSymbol [] CarnevaleOfHorrors

bridgeOfSighs :: CardDef 'LocationType
bridgeOfSighs = location
  "82013"
  "Bridge of Sighs"
  [Venice, Bridge]
  NoSymbol
  []
  CarnevaleOfHorrors

floodedSquare :: CardDef 'LocationType
floodedSquare =
  location "82014" "Flooded Square" [Venice] NoSymbol [] CarnevaleOfHorrors

accademiaBridge :: CardDef 'LocationType
accademiaBridge = location
  "82015"
  "Accademia Bridge"
  [Venice, Bridge]
  NoSymbol
  []
  CarnevaleOfHorrors

theGuardian :: CardDef 'LocationType
theGuardian =
  location "82016" "The Guardian" [Venice] NoSymbol [] CarnevaleOfHorrors

betweenWorlds :: CardDef 'LocationType
betweenWorlds =
  location "xbetween" "Between Worlds" [Hex] NoSymbol [] ShatteredAeons
