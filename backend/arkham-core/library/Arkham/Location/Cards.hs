module Arkham.Location.Cards where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet hiding (Dunwich)
import Arkham.Keyword qualified as Keyword
import Arkham.LocationSymbol
import Arkham.Name
import Arkham.Trait hiding (Circle)
import Arkham.Trait qualified as Trait

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
  -> CardDef
locationWithUnrevealed cardCode unrevealedName unrevealedTraits unrevealedLocationSymbol unrevealedConnectedLocationSymbols name traits locationSymbol connectedLocationSymbols encounterSet =
  ( location
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
  -> CardDef
location cardCode name traits locationSymbol connectedLocationSymbols encounterSet =
  CardDef
    { cdCardCode = cardCode
    , cdName = name
    , cdRevealedName = Just name
    , cdCost = Nothing
    , cdAdditionalCost = Nothing
    , cdLevel = 0
    , cdCardType = LocationType
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
    , cdDeckRestrictions = []
    }

allLocationCards :: Map CardCode CardDef
allLocationCards =
  mapFromList $
    map
      (toCardCode &&& id)
      [ aPocketInTime
      , aTearInThePath
      , abandonedChapel
      , abandonedChapelSpectral
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
      , balconyAtDeathsDoorstep
      , balconySpectral
      , ballroom
      , baseOfTheHill
      , basementHall
      , bathroom
      , bedroom
      , billiardsRoom
      , billiardsRoomSpectral
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
      , chapelAtticSpectral_175
      , chapelAtticSpectral_176
      , chapelAttic_175
      , chapelAttic_176
      , chapelCryptSpectral_173
      , chapelCryptSpectral_174
      , chapelCrypt_173
      , chapelCrypt_174
      , chapultepecHill_178
      , chapultepecHill_179
      , chapultepecPark
      , choeurGothique_292
      , choeurGothique_293
      , circuitousTrail
      , cityOfElderThings
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
      , courtOfTheGreatOldOnes
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
      , entryHallAtDeathsDoorstep
      , entryHallSpectral
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
      , faubourgMarigny
      , floodedSquare
      , forbiddingShore
      , forgottenMarsh
      , forkedPath
      , foulSwamp
      , foyer
      , frankElwoodsRoom
      , frenchHill_290
      , frenchHill_291
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
      , hangmansBrook
      , hangmansBrookSpectral
      , hauntedFields
      , hauntedFieldsSpectral
      , hereticsGravesSpectral_171
      , hereticsGravesSpectral_172
      , hereticsGraves_171
      , hereticsGraves_172
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
      , innerSanctum
      , interviewRoomArrivalChamber
      , interviewRoomIchorFilledChamber
      , interviewRoomRestrainingChamber
      , joeMazurewiczsRoom
      , keziahsRoom
      , kitchen
      , knightsHall
      , laBellaLuna
      , laboratoryOfTheGreatRace
      , labyrinthOfBones
      , lakeXochimilco_182
      , lakeXochimilco_183
      , landlordsQuarters
      , leMarais217
      , leMarais218
      , library
      , lightingBox
      , livingRoom
      , lobby
      , lobbyMembersOnly
      , lobbyWeveBeenExpectingYou
      , lodgeCatacombs
      , lodgeCellarMembersOnly
      , lodgeCellarWeveBeenExpectingYou
      , lodgeGatesMembersOnly
      , lodgeGatesWeveBeenExpectingYou
      , lostMemories
      , lounge
      , mainPath
      , masterBedroom
      , masterBedroomSpectral
      , merchantDistrict_300
      , merchantDistrict_301
      , messHall
      , metropolitanCathedral
      , miskatonicQuad
      , miskatonicRiver
      , miskatonicUniversity
      , miskatonicUniversityMiskatonicMuseum
      , moldyHalls
      , moldyHallsEarlierTonight
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
      , office
      , officeSpectral
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
      , physicsClassroom
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
      , riverCanyon
      , rivertown
      , rivertownAbandonedWarehouse
      , rivertown_292
      , rivertown_293
      , ropeBridge
      , ruinsOfCarcosaAMomentsRest
      , ruinsOfCarcosaInhabitantOfCarcosa
      , ruinsOfCarcosaTheCoffin
      , ruinsOfEztli
      , ruinsOfNewYork
      , sacredWoods_184
      , sacredWoods_185
      , salemGaol1692
      , sanMarcoBasilica
      , sanctumDoorwayCeremonyRoom
      , sanctumDoorwayHoldingCells
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
      , siteOfTheSacrifice
      , slaughteredWoods
      , sleepingCar
      , southChurch_298
      , southChurch_299
      , southsideHistoricalSociety
      , southsideMasBoardingHouse
      , southside_294
      , southside_295
      , stMarysHospital
      , stepsOfYhagharl
      , stepsOfYoth
      , stoneAltar
      , stoneArchways
      , strangeGeometry
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
      , theGeistTrap
      , theGuardian
      , theHiddenChamber
      , theatre
      , theGallowsSpectral_169
      , theGallowsSpectral_170
      , theGallows_169
      , theGallows_170
      , timeWrackedWoods
      , tombOfShadows
      , towersOfPnakotus
      , townHall
      , trainTracks
      , trapRoom
      , trappersCabin
      , trophyRoom
      , trophyRoomSpectral
      , twilightAbyss
      , twistedUnderbrush
      , undergroundRuins
      , unvisitedIsleDecayedWillow
      , unvisitedIsleForsakenWoods
      , unvisitedIsleHauntedSpring
      , unvisitedIsleMistyClearing
      , unvisitedIsleMossCoveredSteps
      , unvisitedIsleStandingStones
      , uprootedWoods
      , uptown_296
      , uptown_297
      , valusia
      , vastPassages
      , vault
      , velmasDiner
      , venetianGarden
      , victorianHalls
      , victorianHallsSpectral
      , villageCommons
      , vipArea
      , walterGilmansRoom
      , wellOfSouls
      , whateleyRuins_250
      , whateleyRuins_251
      , witchHauntedWoodsAbandonedMine
      , witchHauntedWoodsCairnStones
      , witchHauntedWoodsChildsTreeHouse
      , witchHauntedWoodsHermitsHouse
      , witchHauntedWoodsOvergrownBarn
      , witchHauntedWoodsTaintedWell
      , witchHauntedWoodsTheLonelyTree
      , witchHouseRuins
      , witchesCircle
      , xochimilco
      , yard
      , yithianOrrery
      , yourHouse
      , yuggoth
      , zocalo
      ]

allSpecialLocationCards :: Map CardCode CardDef
allSpecialLocationCards =
  mapFromList $ map (toCardCode &&& id) [betweenWorlds]

vengeance :: Int -> CardDef -> CardDef
vengeance n def = def {cdVengeancePoints = Just n}

victory :: Int -> CardDef -> CardDef
victory n def = def {cdVictoryPoints = Just n}

revelation :: CardDef -> CardDef
revelation def = def {cdRevelation = True}

singleSided :: CardDef -> CardDef
singleSided def = def {cdDoubleSided = False}

quantity :: Int -> CardDef -> CardDef
quantity n def = def {cdEncounterSetQuantity = Just n}

study :: CardDef
study = location "01111" "Study" mempty Circle [] TheGathering

hallway :: CardDef
hallway =
  location
    "01112"
    "Hallway"
    mempty
    Square
    [Triangle, Plus, Diamond]
    TheGathering

attic :: CardDef
attic =
  victory 1 $ location "01113" "Attic" mempty Triangle [Square] TheGathering

cellar :: CardDef
cellar =
  victory 1 $ location "01114" "Cellar" mempty Plus [Square] TheGathering

parlor :: CardDef
parlor = location "01115" "Parlor" mempty Diamond [Square] TheGathering

yourHouse :: CardDef
yourHouse =
  location "01124" "Your House" [Arkham] Squiggle [Circle] TheMidnightMasks

rivertown :: CardDef
rivertown =
  location
    "01125"
    "Rivertown"
    [Arkham, Central]
    Circle
    [Moon, Diamond, Square, Squiggle, Hourglass]
    TheMidnightMasks

southsideHistoricalSociety :: CardDef
southsideHistoricalSociety =
  location
    "01126"
    ("Southside" <:> "Historical Society")
    [Arkham]
    Square
    [Diamond, Plus, Circle]
    TheMidnightMasks

southsideMasBoardingHouse :: CardDef
southsideMasBoardingHouse =
  location
    "01127"
    ("Southside" <:> "Ma's Boarding House")
    [Arkham]
    Square
    [Diamond, Plus, Circle]
    TheMidnightMasks

stMarysHospital :: CardDef
stMarysHospital =
  location
    "01128"
    "St. Mary's Hospital"
    [Arkham]
    Plus
    [Diamond, Square]
    TheMidnightMasks

miskatonicUniversity :: CardDef
miskatonicUniversity =
  victory 1 $
    location
      "01129"
      "Miskatonic University"
      [Arkham]
      Diamond
      [T, Plus, Circle, Square]
      TheMidnightMasks

downtownFirstBankOfArkham :: CardDef
downtownFirstBankOfArkham =
  location
    "01130"
    ("Downtown" <:> "First Bank of Arkham")
    [Arkham]
    Triangle
    [Moon, T]
    TheMidnightMasks

downtownArkhamAsylum :: CardDef
downtownArkhamAsylum =
  victory 1 $
    location
      "01131"
      ("Downtown" <:> "Arkham Asylum")
      [Arkham]
      Triangle
      [Moon, T]
      TheMidnightMasks

easttown :: CardDef
easttown =
  location "01132" "Easttown" [Arkham] Moon [Circle, Triangle] TheMidnightMasks

graveyard :: CardDef
graveyard =
  victory 1 $
    location "01133" "Graveyard" [Arkham] Hourglass [Circle] TheMidnightMasks

northside :: CardDef
northside =
  victory 1 $
    location
      "01134"
      "Northside"
      [Arkham]
      T
      [Diamond, Triangle]
      TheMidnightMasks

mainPath :: CardDef
mainPath =
  location "01149" "Main Path" [Woods] Squiggle [Square, Plus] TheDevourerBelow

arkhamWoodsUnhallowedGround :: CardDef
arkhamWoodsUnhallowedGround =
  locationWithUnrevealed
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

arkhamWoodsTwistingPaths :: CardDef
arkhamWoodsTwistingPaths =
  locationWithUnrevealed
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

arkhamWoodsOldHouse :: CardDef
arkhamWoodsOldHouse =
  locationWithUnrevealed
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

arkhamWoodsCliffside :: CardDef
arkhamWoodsCliffside =
  locationWithUnrevealed
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

arkhamWoodsTangledThicket :: CardDef
arkhamWoodsTangledThicket =
  locationWithUnrevealed
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

arkhamWoodsQuietGlade :: CardDef
arkhamWoodsQuietGlade =
  locationWithUnrevealed
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

ritualSite :: CardDef
ritualSite =
  location "01156" "Ritual Site" [Cave] Plus [Squiggle] TheDevourerBelow

miskatonicQuad :: CardDef
miskatonicQuad =
  location
    "02048"
    "Miskatonic Quad"
    [Miskatonic]
    Plus
    [Triangle, Hourglass, Square, Diamond, Circle]
    ExtracurricularActivity

humanitiesBuilding :: CardDef
humanitiesBuilding =
  location
    "02049"
    "Humanities Building"
    [Miskatonic]
    Square
    [Plus, Triangle]
    ExtracurricularActivity

orneLibrary :: CardDef
orneLibrary =
  victory 1 $
    location
      "02050"
      "Orne Library"
      [Miskatonic]
      Triangle
      [Plus, Square]
      ExtracurricularActivity

studentUnion :: CardDef
studentUnion =
  location
    "02051"
    "Student Union"
    [Miskatonic]
    Diamond
    [Plus, Equals]
    ExtracurricularActivity

dormitories :: CardDef
dormitories =
  victory 1 $
    location
      "02052"
      "Dormitories"
      [Miskatonic]
      Equals
      [Diamond]
      ExtracurricularActivity

administrationBuilding :: CardDef
administrationBuilding =
  location
    "02053"
    "Administration Building"
    [Miskatonic]
    Circle
    [Plus, T]
    ExtracurricularActivity

facultyOfficesTheNightIsStillYoung :: CardDef
facultyOfficesTheNightIsStillYoung =
  victory 1 $
    location
      "02054"
      ("Faculty Offices" <:> "The Night is Still Young")
      [Miskatonic]
      T
      [Circle]
      ExtracurricularActivity

facultyOfficesTheHourIsLate :: CardDef
facultyOfficesTheHourIsLate =
  location
    "02055"
    ("Faculty Offices" <:> "The Hour is Late")
    [Miskatonic]
    T
    [Circle]
    ExtracurricularActivity

scienceBuilding :: CardDef
scienceBuilding =
  location
    "02056"
    "Science Building"
    [Miskatonic]
    Hourglass
    [Plus, Squiggle]
    ExtracurricularActivity

alchemyLabs :: CardDef
alchemyLabs =
  location
    "02057"
    "Alchemy Labs"
    [Miskatonic]
    Squiggle
    [Hourglass]
    ExtracurricularActivity

laBellaLuna :: CardDef
laBellaLuna =
  location "02070" "La Bella Luna" [Arkham] Moon [Circle] TheHouseAlwaysWins

cloverClubLounge :: CardDef
cloverClubLounge =
  location
    "02071"
    "Clover Club Lounge"
    [CloverClub]
    Circle
    [Moon, Square, Triangle]
    TheHouseAlwaysWins

cloverClubBar :: CardDef
cloverClubBar =
  location
    "02072"
    "Clover Club Bar"
    [CloverClub]
    Square
    [Triangle, Circle]
    TheHouseAlwaysWins

cloverClubCardroom :: CardDef
cloverClubCardroom =
  location
    "02073"
    "Clover Club Cardroom"
    [CloverClub]
    Triangle
    [Circle, Square, Diamond]
    TheHouseAlwaysWins

darkenedHall :: CardDef
darkenedHall =
  location
    "02074"
    "Darkened Hall"
    [CloverClub]
    Diamond
    [Triangle, T, Hourglass, Plus, Squiggle]
    TheHouseAlwaysWins

artGallery :: CardDef
artGallery =
  victory 1 $
    locationWithUnrevealed
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

vipArea :: CardDef
vipArea =
  victory 1 $
    locationWithUnrevealed
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

backAlley :: CardDef
backAlley =
  victory 1 $
    locationWithUnrevealed
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

museumEntrance :: CardDef
museumEntrance =
  location
    "02126"
    "Museum Entrance"
    [Miskatonic]
    Circle
    [Square]
    TheMiskatonicMuseum

museumHalls :: CardDef
museumHalls =
  location
    "02127"
    "Museum Halls"
    [Miskatonic]
    Square
    [Circle, Diamond, Triangle]
    TheMiskatonicMuseum

securityOffice_128 :: CardDef
securityOffice_128 =
  location
    "02128"
    "Security Office"
    [Miskatonic]
    Diamond
    [Square]
    TheMiskatonicMuseum

securityOffice_129 :: CardDef
securityOffice_129 =
  location
    "02129"
    "Security Office"
    [Miskatonic]
    Diamond
    [Square]
    TheMiskatonicMuseum

administrationOffice_130 :: CardDef
administrationOffice_130 =
  location
    "02130"
    "Administration Office"
    [Miskatonic]
    Triangle
    [Square]
    TheMiskatonicMuseum

administrationOffice_131 :: CardDef
administrationOffice_131 =
  location
    "02131"
    "Administration Office"
    [Miskatonic]
    Triangle
    [Square]
    TheMiskatonicMuseum

exhibitHallAthabaskanExhibit :: CardDef
exhibitHallAthabaskanExhibit =
  locationWithUnrevealed
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

exhibitHallMedusaExhibit :: CardDef
exhibitHallMedusaExhibit =
  victory 1 $
    locationWithUnrevealed
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

exhibitHallNatureExhibit :: CardDef
exhibitHallNatureExhibit =
  victory 1 $
    locationWithUnrevealed
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

exhibitHallEgyptianExhibit :: CardDef
exhibitHallEgyptianExhibit =
  victory 1 $
    locationWithUnrevealed
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

exhibitHallHallOfTheDead :: CardDef
exhibitHallHallOfTheDead =
  victory 1 $
    locationWithUnrevealed
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

exhibitHallRestrictedHall :: CardDef
exhibitHallRestrictedHall =
  victory 1 $
    locationWithUnrevealed
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

passengerCar_167 :: CardDef
passengerCar_167 =
  locationWithUnrevealed
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

passengerCar_168 :: CardDef
passengerCar_168 =
  locationWithUnrevealed
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

passengerCar_169 :: CardDef
passengerCar_169 =
  locationWithUnrevealed
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

passengerCar_170 :: CardDef
passengerCar_170 =
  locationWithUnrevealed
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

passengerCar_171 :: CardDef
passengerCar_171 =
  locationWithUnrevealed
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

sleepingCar :: CardDef
sleepingCar =
  locationWithUnrevealed
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

diningCar :: CardDef
diningCar =
  locationWithUnrevealed
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

parlorCar :: CardDef
parlorCar =
  victory 1 $
    locationWithUnrevealed
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

engineCar_175 :: CardDef
engineCar_175 =
  victory 1 $
    location "02175" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress

engineCar_176 :: CardDef
engineCar_176 =
  victory 1 $
    location "02176" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress

engineCar_177 :: CardDef
engineCar_177 =
  victory 1 $
    location "02177" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress

villageCommons :: CardDef
villageCommons =
  location
    "02201"
    "Village Commons"
    [Dunwich, Central]
    Plus
    [Square, Circle, Moon]
    BloodOnTheAltar

bishopsBrook_202 :: CardDef
bishopsBrook_202 =
  location
    "02202"
    "Bishop's Brook"
    [Dunwich]
    Square
    [Plus, Circle, Triangle]
    BloodOnTheAltar

bishopsBrook_203 :: CardDef
bishopsBrook_203 =
  location
    "02203"
    "Bishop's Brook"
    [Dunwich]
    Square
    [Plus, Circle, Triangle]
    BloodOnTheAltar

burnedRuins_204 :: CardDef
burnedRuins_204 =
  location
    "02204"
    "Burned Ruins"
    [Dunwich]
    Triangle
    [Square, Diamond]
    BloodOnTheAltar

burnedRuins_205 :: CardDef
burnedRuins_205 =
  location
    "02205"
    "Burned Ruins"
    [Dunwich]
    Triangle
    [Square, Diamond]
    BloodOnTheAltar

osbornsGeneralStore_206 :: CardDef
osbornsGeneralStore_206 =
  location
    "02206"
    "Osborn's General Store"
    [Dunwich]
    Circle
    [Moon, Square]
    BloodOnTheAltar

osbornsGeneralStore_207 :: CardDef
osbornsGeneralStore_207 =
  location
    "02207"
    "Osborn's General Store"
    [Dunwich]
    Circle
    [Moon, Square]
    BloodOnTheAltar

congregationalChurch_208 :: CardDef
congregationalChurch_208 =
  location
    "02208"
    "Congregational Church"
    [Dunwich]
    Diamond
    [Plus, Triangle, Squiggle]
    BloodOnTheAltar

congregationalChurch_209 :: CardDef
congregationalChurch_209 =
  location
    "02209"
    "Congregational Church"
    [Dunwich]
    Diamond
    [Plus, Triangle, Squiggle]
    BloodOnTheAltar

houseInTheReeds_210 :: CardDef
houseInTheReeds_210 =
  location
    "02210"
    "House in the Reeds"
    [Dunwich]
    Squiggle
    [Diamond, Moon]
    BloodOnTheAltar

houseInTheReeds_211 :: CardDef
houseInTheReeds_211 =
  location
    "02211"
    "House in the Reeds"
    [Dunwich]
    Squiggle
    [Diamond, Moon]
    BloodOnTheAltar

schoolhouse_212 :: CardDef
schoolhouse_212 =
  location
    "02212"
    "Schoolhouse"
    [Dunwich]
    Moon
    [Plus, Squiggle, Moon]
    BloodOnTheAltar

schoolhouse_213 :: CardDef
schoolhouse_213 =
  location
    "02213"
    "Schoolhouse"
    [Dunwich]
    Moon
    [Plus, Squiggle, Moon]
    BloodOnTheAltar

theHiddenChamber :: CardDef
theHiddenChamber =
  victory 2 $
    location
      "02214"
      ("The Hidden Chamber" <:> "Prison of the Beast")
      [Dunwich]
      NoSymbol
      []
      BloodOnTheAltar

dunwichVillage_242 :: CardDef
dunwichVillage_242 =
  location
    "02242"
    "Dunwich Village"
    [Dunwich]
    Circle
    [Triangle, Square, Diamond]
    UndimensionedAndUnseen

dunwichVillage_243 :: CardDef
dunwichVillage_243 =
  location
    "02243"
    "Dunwich Village"
    [Dunwich]
    Circle
    [Triangle, Square, Diamond]
    UndimensionedAndUnseen

coldSpringGlen_244 :: CardDef
coldSpringGlen_244 =
  location
    "02244"
    "Cold Spring Glen"
    [Dunwich]
    Triangle
    [Circle, Diamond, Plus]
    UndimensionedAndUnseen

coldSpringGlen_245 :: CardDef
coldSpringGlen_245 =
  location
    "02245"
    "Cold Spring Glen"
    [Dunwich]
    Triangle
    [Circle, Diamond, Plus]
    UndimensionedAndUnseen

tenAcreMeadow_246 :: CardDef
tenAcreMeadow_246 =
  location
    "02246"
    "Ten-Acre Meadow"
    [Dunwich]
    Diamond
    [Circle, Triangle, Plus]
    UndimensionedAndUnseen

tenAcreMeadow_247 :: CardDef
tenAcreMeadow_247 =
  location
    "02247"
    "Ten-Acre Meadow"
    [Dunwich]
    Diamond
    [Circle, Triangle, Plus]
    UndimensionedAndUnseen

blastedHeath_248 :: CardDef
blastedHeath_248 =
  location
    "02248"
    "Blasted Heath"
    [Dunwich]
    Square
    [Circle, Hourglass]
    UndimensionedAndUnseen

blastedHeath_249 :: CardDef
blastedHeath_249 =
  location
    "02249"
    "Blasted Heath"
    [Dunwich]
    Square
    [Circle, Hourglass]
    UndimensionedAndUnseen

whateleyRuins_250 :: CardDef
whateleyRuins_250 =
  location
    "02250"
    "Whateley Ruins"
    [Dunwich]
    Plus
    [Triangle, Diamond, Hourglass]
    UndimensionedAndUnseen

whateleyRuins_251 :: CardDef
whateleyRuins_251 =
  location
    "02251"
    "Whateley Ruins"
    [Dunwich]
    Plus
    [Triangle, Diamond, Hourglass]
    UndimensionedAndUnseen

devilsHopYard_252 :: CardDef
devilsHopYard_252 =
  location
    "02252"
    "Devil's Hop Yard"
    [Dunwich]
    Hourglass
    [Square, Plus]
    UndimensionedAndUnseen

devilsHopYard_253 :: CardDef
devilsHopYard_253 =
  location
    "02253"
    "Devil's Hop Yard"
    [Dunwich]
    Hourglass
    [Square, Plus]
    UndimensionedAndUnseen

baseOfTheHill :: CardDef
baseOfTheHill =
  location
    "02282"
    "Base of the Hill"
    [Dunwich, SentinelHill]
    Triangle
    [Square, Plus, Squiggle, Hourglass]
    WhereDoomAwaits

ascendingPath :: CardDef
ascendingPath =
  location
    "02283"
    "Ascending Path"
    [Dunwich, SentinelHill]
    Square
    [Triangle, Diamond, T, Equals, Moon]
    WhereDoomAwaits

sentinelPeak :: CardDef
sentinelPeak =
  victory 2 $
    location
      "02284"
      "Sentinel Peak"
      [Dunwich, SentinelHill]
      Diamond
      [Square]
      WhereDoomAwaits

slaughteredWoods :: CardDef
slaughteredWoods =
  locationWithUnrevealed
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

eerieGlade :: CardDef
eerieGlade =
  locationWithUnrevealed
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

destroyedPath :: CardDef
destroyedPath =
  locationWithUnrevealed
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

frozenSpring :: CardDef
frozenSpring =
  locationWithUnrevealed
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

dimensionalGap :: CardDef
dimensionalGap =
  locationWithUnrevealed
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

aTearInThePath :: CardDef
aTearInThePath =
  locationWithUnrevealed
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

uprootedWoods :: CardDef
uprootedWoods =
  locationWithUnrevealed
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

lostMemories :: CardDef
lostMemories =
  locationWithUnrevealed
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

anotherDimension :: CardDef
anotherDimension =
  location
    "02320"
    ("Another Dimension" <:> "Unfettered by Reality")
    [Otherworld]
    Circle
    [Square, Diamond, Triangle]
    LostInTimeAndSpace

theEdgeOfTheUniverse :: CardDef
theEdgeOfTheUniverse =
  location
    "02321"
    "The Edge of the Universe"
    [Otherworld]
    Moon
    [Plus, Squiggle]
    LostInTimeAndSpace

tearThroughTime :: CardDef
tearThroughTime =
  location
    "02322"
    "Tear Through Time"
    [Otherworld]
    Moon
    [Circle, Plus, Squiggle]
    LostInTimeAndSpace

tearThroughSpace :: CardDef
tearThroughSpace =
  singleSided $
    ( location
        "02324"
        "Tear Through Space"
        [Otherworld, Extradimensional]
        Square
        [Diamond, Triangle, Square]
        LostInTimeAndSpace
    )
      { cdKeywords = setFromList [Keyword.Surge]
      , cdEncounterSetQuantity = Just 4
      }

prismaticCascade :: CardDef
prismaticCascade =
  singleSided $
    ( location
        "02325"
        "Prismatic Cascade"
        [Otherworld, Extradimensional]
        Diamond
        [Square, Plus]
        LostInTimeAndSpace
    )
      { cdEncounterSetQuantity = Just 2
      }

endlessBridge :: CardDef
endlessBridge =
  singleSided $
    ( location
        "02326"
        "Endless Bridge"
        [Otherworld, Extradimensional]
        Triangle
        [Square, Squiggle]
        LostInTimeAndSpace
    )
      { cdEncounterSetQuantity = Just 2
      }

stepsOfYhagharl :: CardDef
stepsOfYhagharl =
  singleSided $
    location
      "02327"
      "Steps of Y'hagharl"
      [Otherworld, Extradimensional]
      Plus
      [Diamond, Moon]
      LostInTimeAndSpace

dimensionalDoorway :: CardDef
dimensionalDoorway =
  singleSided $
    location
      "02328"
      "Dimensional Doorway"
      [Otherworld, Extradimensional]
      Squiggle
      [Triangle, Moon]
      LostInTimeAndSpace

theatre :: CardDef
theatre =
  location "03049" "Theatre" mempty Circle [Diamond, Triangle] CurtainCall

lobby :: CardDef
lobby =
  location "03050" "Lobby" mempty Triangle [Circle, Square, Plus] CurtainCall

balcony :: CardDef
balcony =
  victory 1 $
    location "03051" "Balcony" mempty Square [Circle, Triangle] CurtainCall

backstage :: CardDef
backstage =
  location "03052" "Backstage" mempty Diamond [Circle, Moon] CurtainCall

lightingBox :: CardDef
lightingBox =
  victory 1 $
    locationWithUnrevealed
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

boxOffice :: CardDef
boxOffice =
  locationWithUnrevealed
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

greenRoom :: CardDef
greenRoom =
  victory 1 $
    locationWithUnrevealed
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

dressingRoom :: CardDef
dressingRoom =
  locationWithUnrevealed
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

rehearsalRoom :: CardDef
rehearsalRoom =
  victory 1 $
    locationWithUnrevealed
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

trapRoom :: CardDef
trapRoom =
  victory 1 $
    locationWithUnrevealed
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

foyer :: CardDef
foyer = location "03070" "Foyer" mempty T [Circle, Square, Equals] TheLastKing

ballroom :: CardDef
ballroom =
  location "03071" "Ballroom" mempty Square [T, Circle, Squiggle] TheLastKing

livingRoom :: CardDef
livingRoom =
  location "03072" "Living Room" mempty Equals [T, Circle, Plus] TheLastKing

gallery :: CardDef
gallery = location "03073" "Gallery" mempty Plus [Equals, Circle] TheLastKing

courtyard :: CardDef
courtyard =
  location
    "03074"
    "Courtyard"
    mempty
    Circle
    [Squiggle, Square, T, Equals, Plus]
    TheLastKing

diningRoom :: CardDef
diningRoom =
  location "03075" "Dining Room" mempty Squiggle [Square, Circle] TheLastKing

entryHall :: CardDef
entryHall =
  location "03127" "Entry Hall" [GroundFloor] Square [Circle] EchoesOfThePast

historicalSocietyMeetingRoom :: CardDef
historicalSocietyMeetingRoom =
  locationWithUnrevealed
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

historicalSocietyRecordOffice_129 :: CardDef
historicalSocietyRecordOffice_129 =
  locationWithUnrevealed
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

historicalSocietyHistoricalMuseum_130 :: CardDef
historicalSocietyHistoricalMuseum_130 =
  locationWithUnrevealed
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

quietHalls_131 :: CardDef
quietHalls_131 =
  location
    "03131"
    "Quiet Halls"
    [SecondFloor]
    Circle
    [Square, Star]
    EchoesOfThePast

historicalSocietyHistoricalMuseum_132 :: CardDef
historicalSocietyHistoricalMuseum_132 =
  locationWithUnrevealed
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

historicalSocietyHistoricalLibrary_133 :: CardDef
historicalSocietyHistoricalLibrary_133 =
  locationWithUnrevealed
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

historicalSocietyReadingRoom :: CardDef
historicalSocietyReadingRoom =
  locationWithUnrevealed
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

quietHalls_135 :: CardDef
quietHalls_135 =
  location "03135" "Quiet Halls" [ThirdFloor] Star [Circle] EchoesOfThePast

historicalSocietyHistoricalLibrary_136 :: CardDef
historicalSocietyHistoricalLibrary_136 =
  locationWithUnrevealed
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

historicalSocietyPeabodysOffice :: CardDef
historicalSocietyPeabodysOffice =
  locationWithUnrevealed
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

historicalSocietyRecordOffice_138 :: CardDef
historicalSocietyRecordOffice_138 =
  locationWithUnrevealed
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

hiddenLibrary :: CardDef
hiddenLibrary =
  victory 2 $
    location "03139" "Hidden Library" mempty NoSymbol [] EchoesOfThePast

asylumHallsWesternPatientWing_168 :: CardDef
asylumHallsWesternPatientWing_168 =
  location
    "03168"
    ("Asylum Halls" <:> "Western Patient Wing")
    [ArkhamAsylum]
    Circle
    [Hourglass, Triangle, Diamond]
    TheUnspeakableOath

asylumHallsWesternPatientWing_169 :: CardDef
asylumHallsWesternPatientWing_169 =
  location
    "03169"
    ("Asylum Halls" <:> "Western Patient Wing")
    [ArkhamAsylum]
    Circle
    [Hourglass, Triangle, Diamond]
    TheUnspeakableOath

asylumHallsEasternPatientWing_170 :: CardDef
asylumHallsEasternPatientWing_170 =
  location
    "03170"
    ("Asylum Halls" <:> "Eastern Patient Wing")
    [ArkhamAsylum]
    Hourglass
    [Circle, Heart, Squiggle]
    TheUnspeakableOath

asylumHallsEasternPatientWing_171 :: CardDef
asylumHallsEasternPatientWing_171 =
  location
    "03171"
    ("Asylum Halls" <:> "Eastern Patient Wing")
    [ArkhamAsylum]
    Hourglass
    [Circle, Heart, Squiggle]
    TheUnspeakableOath

kitchen :: CardDef
kitchen =
  location "03172" "Kitchen" [ArkhamAsylum] Square [Triangle] TheUnspeakableOath

messHall :: CardDef
messHall =
  victory 1 $
    location
      "03173"
      "Mess Hall"
      [ArkhamAsylum]
      Triangle
      [Circle, Square]
      TheUnspeakableOath

infirmary :: CardDef
infirmary =
  victory 1 $
    location
      "03174"
      "Infirmary"
      [ArkhamAsylum]
      Heart
      [Hourglass]
      TheUnspeakableOath

yard :: CardDef
yard =
  location
    "03175"
    "Yard"
    [ArkhamAsylum]
    Diamond
    [Circle, Plus]
    TheUnspeakableOath

garden :: CardDef
garden =
  location "03176" "Garden" [ArkhamAsylum] Plus [Diamond] TheUnspeakableOath

basementHall :: CardDef
basementHall =
  victory 1 $
    location
      "03177"
      "Basement Hall"
      [ArkhamAsylum]
      Squiggle
      [Hourglass, Moon]
      TheUnspeakableOath

patientConfinementDanielsCell :: CardDef
patientConfinementDanielsCell =
  locationWithUnrevealed
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

patientConfinementOccupiedCell :: CardDef
patientConfinementOccupiedCell =
  locationWithUnrevealed
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

patientConfinementDrearyCell :: CardDef
patientConfinementDrearyCell =
  locationWithUnrevealed
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

patientConfinementFamiliarCell :: CardDef
patientConfinementFamiliarCell =
  locationWithUnrevealed
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

montparnasse :: CardDef
montparnasse =
  location
    "03208"
    "Montparnasse"
    [Paris, Rail]
    Circle
    [Heart, Star, Plus]
    APhantomOfTruth

montmartre209 :: CardDef
montmartre209 =
  location
    "03209"
    "Montmartre"
    [Paris, Rail]
    Square
    [Diamond, Triangle, Equals, Moon]
    APhantomOfTruth

montmartre210 :: CardDef
montmartre210 =
  location
    "03210"
    "Montmartre"
    [Paris, Rail]
    Square
    [Diamond, Triangle, Equals, Moon]
    APhantomOfTruth

grandGuignol :: CardDef
grandGuignol =
  victory 1 $
    location
      "03211"
      ("Grand Guignol" <:> "Theatre of the Great Puppet")
      [Paris]
      Triangle
      [Diamond, Square]
      APhantomOfTruth

operaGarnier212 :: CardDef
operaGarnier212 =
  location
    "03212"
    "Opéra Garnier"
    [Paris, Rail]
    Diamond
    [Triangle, Square, Heart]
    APhantomOfTruth

operaGarnier213 :: CardDef
operaGarnier213 =
  location
    "03213"
    "Opéra Garnier"
    [Paris, Rail]
    Diamond
    [Triangle, Square, Heart]
    APhantomOfTruth

gareDOrsay :: CardDef
gareDOrsay =
  location
    "03214"
    "Gare d'Orsay"
    [Paris, Rail]
    Heart
    [Diamond, Circle, Star]
    APhantomOfTruth

pereLachaiseCemetery :: CardDef
pereLachaiseCemetery =
  victory 1 $
    location
      "03215"
      "Père Lachaise Cemetery"
      [Paris]
      T
      [Equals, Moon]
      APhantomOfTruth

canalSaintMartin :: CardDef
canalSaintMartin =
  victory 1 $
    location
      "03216"
      "Canal Saint-Martin"
      [Paris]
      Equals
      [Square, T, Moon]
      APhantomOfTruth

leMarais217 :: CardDef
leMarais217 =
  location
    "03217"
    "Le Marais"
    [Paris, Rail]
    Moon
    [Square, Equals, T, Plus]
    APhantomOfTruth

leMarais218 :: CardDef
leMarais218 =
  location
    "03218"
    "Le Marais"
    [Paris, Rail]
    Moon
    [Square, Equals, T, Plus]
    APhantomOfTruth

notreDame :: CardDef
notreDame =
  location
    "03219"
    "Notre-Dame"
    [Paris, Rail]
    Plus
    [Circle, Moon, Star]
    APhantomOfTruth

gardensOfLuxembourg :: CardDef
gardensOfLuxembourg =
  victory 1 $
    location
      "03220"
      "Gardens of Luxembourg"
      [Paris]
      Star
      [Circle, Heart, Plus]
      APhantomOfTruth

theGateToHell :: CardDef
theGateToHell =
  locationWithUnrevealed
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

stoneArchways :: CardDef
stoneArchways =
  quantity 2 $
    locationWithUnrevealed
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

cryptOfTheSepulchralLamp :: CardDef
cryptOfTheSepulchralLamp =
  locationWithUnrevealed
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

boneFilledCaverns :: CardDef
boneFilledCaverns =
  victory 1 $
    locationWithUnrevealed
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

wellOfSouls :: CardDef
wellOfSouls =
  victory 1 $
    locationWithUnrevealed
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

candlelitTunnels :: CardDef
candlelitTunnels =
  quantity 2 $
    locationWithUnrevealed
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

labyrinthOfBones :: CardDef
labyrinthOfBones =
  quantity 2 $
    locationWithUnrevealed
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

narrowShaft :: CardDef
narrowShaft =
  victory 1 $
    locationWithUnrevealed
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

shiveringPools :: CardDef
shiveringPools =
  victory 1 $
    locationWithUnrevealed
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

blockedPassage :: CardDef
blockedPassage =
  locationWithUnrevealed
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

tombOfShadows :: CardDef
tombOfShadows =
  victory 1 $
    locationWithUnrevealed
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

porteDeLAvancee :: CardDef
porteDeLAvancee =
  location "03283" "Porte de l'Avancée" [] Circle [Squiggle] BlackStarsRise

grandRue :: CardDef
grandRue =
  location
    "03284"
    "Grand Rue"
    []
    Squiggle
    [Circle, Triangle, Diamond, Equals]
    BlackStarsRise

outerWall_285 :: CardDef
outerWall_285 =
  victory 1 $
    location
      "03285"
      "Outer Wall"
      []
      Triangle
      [Squiggle, Diamond, Equals]
      BlackStarsRise

outerWall_286 :: CardDef
outerWall_286 =
  victory 1 $
    location
      "03286"
      "Outer Wall"
      []
      Triangle
      [Squiggle, Diamond, Equals]
      BlackStarsRise

northTower_287 :: CardDef
northTower_287 =
  victory 1 $
    location
      "03287"
      "North Tower"
      []
      Diamond
      [Squiggle, Triangle, Equals]
      BlackStarsRise

northTower_288 :: CardDef
northTower_288 =
  victory 1 $
    location
      "03288"
      "North Tower"
      []
      Diamond
      [Squiggle, Triangle, Equals]
      BlackStarsRise

brokenSteps_289 :: CardDef
brokenSteps_289 =
  location
    "03289"
    "Broken Steps"
    []
    Equals
    [Squiggle, Triangle, Diamond, Square]
    BlackStarsRise

brokenSteps_290 :: CardDef
brokenSteps_290 =
  location
    "03290"
    "Broken Steps"
    []
    Equals
    [Squiggle, Triangle, Diamond, Square]
    BlackStarsRise

abbeyChurch :: CardDef
abbeyChurch =
  victory 1 $
    location
      "03291"
      "Abbey Church"
      []
      Square
      [Equals, T, Heart, Hourglass, Moon]
      BlackStarsRise

choeurGothique_292 :: CardDef
choeurGothique_292 =
  location "03292" "Chœur Gothique" [] T [Square, Star] BlackStarsRise

choeurGothique_293 :: CardDef
choeurGothique_293 =
  location "03293" "Chœur Gothique" [] T [Square, Star] BlackStarsRise

cloister :: CardDef
cloister =
  location "03294" "Cloister" [] Heart [Square, Hourglass] BlackStarsRise

knightsHall :: CardDef
knightsHall =
  location "03295" "Knight's Hall" [] Hourglass [Square, Heart] BlackStarsRise

chapelOfStAubertThePathIsOpen :: CardDef
chapelOfStAubertThePathIsOpen =
  locationWithUnrevealed
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

chapelOfStAubertWatersForbidden :: CardDef
chapelOfStAubertWatersForbidden =
  victory 2 $
    locationWithUnrevealed
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

abbeyTowerThePathIsOpen :: CardDef
abbeyTowerThePathIsOpen =
  locationWithUnrevealed
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

abbeyTowerSpiresForbidden :: CardDef
abbeyTowerSpiresForbidden =
  victory 2 $
    locationWithUnrevealed
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

shoresOfHali :: CardDef
shoresOfHali =
  location "03325b" "Shores of Hali" [Otherworld] Circle [Square] DimCarcosa

bleakPlainsStarsOfAldebaran :: CardDef
bleakPlainsStarsOfAldebaran =
  location
    "03326b"
    "Bleak Plains"
    [Otherworld]
    Square
    [Circle, Triangle, Diamond]
    DimCarcosa

bleakPlainsBleakDesolation :: CardDef
bleakPlainsBleakDesolation =
  location
    "03326d"
    "Bleak Plains"
    [Otherworld]
    Square
    [Circle, Triangle, Diamond]
    DimCarcosa

ruinsOfCarcosaInhabitantOfCarcosa :: CardDef
ruinsOfCarcosaInhabitantOfCarcosa =
  location
    "03327b"
    "Ruins of Carcosa"
    [Otherworld]
    Triangle
    [Square, Equals, Star]
    DimCarcosa

ruinsOfCarcosaAMomentsRest :: CardDef
ruinsOfCarcosaAMomentsRest =
  location
    "03327d"
    "Ruins of Carcosa"
    [Otherworld]
    Triangle
    [Square, Equals, Star]
    DimCarcosa

ruinsOfCarcosaTheCoffin :: CardDef
ruinsOfCarcosaTheCoffin =
  location
    "03327f"
    "Ruins of Carcosa"
    [Otherworld]
    Triangle
    [Square, Equals, Star]
    DimCarcosa

dimStreetsMappingTheStreets :: CardDef
dimStreetsMappingTheStreets =
  location
    "03328b"
    "Dim Streets"
    [Otherworld]
    Diamond
    [Square, Equals, Star]
    DimCarcosa

dimStreetsTheKingsParade :: CardDef
dimStreetsTheKingsParade =
  location
    "03328d"
    "Dim Streets"
    [Otherworld]
    Diamond
    [Square, Equals, Star]
    DimCarcosa

dimStreetsTheArchway :: CardDef
dimStreetsTheArchway =
  location
    "03328f"
    "Dim Streets"
    [Otherworld]
    Diamond
    [Square, Equals, Star]
    DimCarcosa

depthsOfDemheTheHeightOfTheDepths :: CardDef
depthsOfDemheTheHeightOfTheDepths =
  location
    "03329b"
    "Depths of Demhe"
    [Otherworld]
    Equals
    [Moon, Triangle, Diamond]
    DimCarcosa

depthsOfDemheStepsOfThePalace :: CardDef
depthsOfDemheStepsOfThePalace =
  location
    "03329d"
    "Depths of Demhe"
    [Otherworld]
    Equals
    [Moon, Triangle, Diamond]
    DimCarcosa

darkSpires :: CardDef
darkSpires =
  location "03330b" "Dark Spires" [Otherworld] Moon [Equals] DimCarcosa

palaceOfTheKing :: CardDef
palaceOfTheKing =
  location
    "03331b"
    "Palace of the King"
    [Otherworld]
    Star
    [Triangle, Diamond]
    DimCarcosa

expeditionCamp :: CardDef
expeditionCamp =
  location
    "04050"
    "Expedition Camp"
    [Campsite, Jungle]
    Circle
    [Square, Diamond, Moon]
    TheUntamedWilds

ruinsOfEztli :: CardDef
ruinsOfEztli =
  victory 2 $
    singleSided $
      location
        "04053"
        "Ruins of Eztli"
        [Ancient, Ruins]
        Hourglass
        [Triangle, Heart]
        TheUntamedWilds

entryway :: CardDef
entryway =
  location
    "04060"
    "Entryway"
    [Ancient, Ruins]
    Circle
    [Square, Star]
    TheDoomOfEztli

ancientHall :: CardDef
ancientHall =
  singleSided $
    location
      "04063"
      "Ancient Hall"
      [Ancient, Ruins]
      Square
      [Circle, Star, Diamond]
      TheDoomOfEztli

grandChamber :: CardDef
grandChamber =
  victory 1 $
    singleSided $
      location
        "04064"
        "Grand Chamber"
        [Ancient, Ruins]
        Star
        [Circle, Square, Triangle]
        TheDoomOfEztli

burialPit :: CardDef
burialPit =
  victory 1 $
    singleSided $
      location
        "04065"
        "Burial Pit"
        [Ancient, Ruins]
        Triangle
        [Star, Diamond, Squiggle]
        TheDoomOfEztli

undergroundRuins :: CardDef
undergroundRuins =
  vengeance 1 $
    singleSided $
      location
        "04066"
        "Underground Ruins"
        [Ancient, Ruins]
        Diamond
        [Square, Triangle, Squiggle]
        TheDoomOfEztli

secretPassage :: CardDef
secretPassage =
  victory 1 $
    singleSided $
      location
        "04067"
        "Secret Passage"
        [Ancient, Ruins]
        Squiggle
        [Diamond, Triangle, Hourglass]
        TheDoomOfEztli

chamberOfTime :: CardDef
chamberOfTime =
  victory 2 $
    vengeance 2 $
      singleSided $
        location
          "04068"
          "Chamber of Time"
          [Forgotten, Ruins]
          Hourglass
          [Squiggle]
          TheDoomOfEztli

pathOfThorns :: CardDef
pathOfThorns =
  singleSided $
    location
      "04069"
      "Path of Thorns"
      [Jungle]
      Square
      [Circle, Diamond, Triangle, Squiggle]
      Rainforest

riverCanyon :: CardDef
riverCanyon =
  singleSided $
    location
      "04070"
      "River Canyon"
      [Jungle]
      Diamond
      [Circle, Moon, Heart, Triangle, Square]
      Rainforest

ropeBridge :: CardDef
ropeBridge =
  singleSided $
    location
      "04071"
      "Rope Bridge"
      [Jungle]
      Moon
      [Circle, Diamond, Heart, T]
      Rainforest

serpentsHaven :: CardDef
serpentsHaven =
  victory 1 $
    singleSided $
      location
        "04072"
        "Serpent's Haven"
        [Jungle]
        Triangle
        [Squiggle, Square, Diamond, Hourglass]
        Rainforest

circuitousTrail :: CardDef
circuitousTrail =
  victory 1 $
    singleSided $
      location
        "04073"
        "Circuitous Trail"
        [Jungle]
        Heart
        [Hourglass, Diamond, Moon, T]
        Rainforest

templeOfTheFang :: CardDef
templeOfTheFang =
  victory 2 $
    singleSided $
      location
        "04074"
        "Temple of the Fang"
        [Ancient, Ruins]
        Squiggle
        [Square, Triangle, Equals]
        Rainforest

overgrownRuins :: CardDef
overgrownRuins =
  victory 2 $
    singleSided $
      location
        "04075"
        "Overgrown Ruins"
        [Ancient, Ruins]
        T
        [Moon, Heart, Equals]
        Rainforest

eztliExhibit :: CardDef
eztliExhibit =
  victory 1 $
    singleSided $
      location
        "04117b"
        "Eztli Exhibit"
        [Miskatonic, Exhibit]
        Plus
        [Diamond]
        ThreadsOfFate

velmasDiner :: CardDef
velmasDiner =
  location "04141" "Velma's Diner" [Arkham] NoSymbol [Moon] ThreadsOfFate

curiositieShoppe :: CardDef
curiositieShoppe =
  victory 1 $
    location "04142" "Curiositie Shoppe" [Arkham] NoSymbol [T] ThreadsOfFate

townHall :: CardDef
townHall =
  victory 1 $
    location "04143" "Town Hall" [Arkham] NoSymbol [Triangle] ThreadsOfFate

arkhamPoliceStation :: CardDef
arkhamPoliceStation =
  victory 1 $
    singleSided $
      location
        "04126b"
        "Arkham Police Station"
        [Arkham]
        NoSymbol
        [Moon]
        ThreadsOfFate

trainTracks :: CardDef
trainTracks =
  singleSided $
    location "04128b" "Train Tracks" [Arkham] NoSymbol [T] ThreadsOfFate

blackCave :: CardDef
blackCave =
  victory 1 $
    singleSided $
      location
        "04133f"
        "Black Cave"
        [Cave]
        Hourglass
        [Circle]
        ThreadsOfFate

templeRuins :: CardDef
templeRuins =
  location
    "04168"
    "Temple Ruins"
    [MexicoCity, PresentDay]
    Circle
    [Diamond, Star]
    TheBoundaryBeyond

metropolitanCathedral :: CardDef
metropolitanCathedral =
  location
    "04169"
    "Metropolitan Cathedral"
    [MexicoCity, PresentDay]
    Square
    [Diamond]
    TheBoundaryBeyond

chapultepecPark :: CardDef
chapultepecPark =
  location
    "04170"
    "Chapultepec Park"
    [MexicoCity, PresentDay]
    Triangle
    [Star]
    TheBoundaryBeyond

zocalo :: CardDef
zocalo =
  location
    "04171"
    "Zócalo"
    [MexicoCity, PresentDay]
    Diamond
    [Heart, Square, Star, Circle]
    TheBoundaryBeyond

xochimilco :: CardDef
xochimilco =
  location
    "04172"
    "Xochimilco"
    [MexicoCity, PresentDay]
    Heart
    [Diamond, Star]
    TheBoundaryBeyond

coyoacan :: CardDef
coyoacan =
  location
    "04173"
    "Coyoacán"
    [MexicoCity, PresentDay]
    Star
    [Diamond, Triangle, Circle, Heart]
    TheBoundaryBeyond

temploMayor_174 :: CardDef
temploMayor_174 =
  singleSided $
    location
      "04174"
      "Templo Mayor"
      [Ancient, Tenochtitlan]
      Circle
      [Square, Triangle]
      TheBoundaryBeyond

temploMayor_175 :: CardDef
temploMayor_175 =
  singleSided $
    location
      "04175"
      "Templo Mayor"
      [Ancient, Tenochtitlan]
      Circle
      [Square, Triangle]
      TheBoundaryBeyond

templesOfTenochtitlan_176 :: CardDef
templesOfTenochtitlan_176 =
  singleSided $
    location
      "04176"
      "Temples of Tenochtitlán"
      [Ancient, Tenochtitlan]
      Square
      [Diamond, Circle]
      TheBoundaryBeyond

templesOfTenochtitlan_177 :: CardDef
templesOfTenochtitlan_177 =
  singleSided $
    location
      "04177"
      "Temples of Tenochtitlán"
      [Ancient, Tenochtitlan]
      Square
      [Diamond, Circle]
      TheBoundaryBeyond

chapultepecHill_178 :: CardDef
chapultepecHill_178 =
  singleSided $
    location
      "04178"
      "Chapultepec Hill"
      [Ancient, Tenochtitlan]
      Triangle
      [Star, Circle]
      TheBoundaryBeyond

chapultepecHill_179 :: CardDef
chapultepecHill_179 =
  singleSided $
    location
      "04179"
      "Chapultepec Hill"
      [Ancient, Tenochtitlan]
      Triangle
      [Star, Circle]
      TheBoundaryBeyond

canalsOfTenochtitlan_180 :: CardDef
canalsOfTenochtitlan_180 =
  singleSided $
    location
      "04180"
      "Canals of Tenochtitlán"
      [Ancient, Tenochtitlan]
      Diamond
      [Heart, Square]
      TheBoundaryBeyond

canalsOfTenochtitlan_181 :: CardDef
canalsOfTenochtitlan_181 =
  singleSided $
    location
      "04181"
      "Canals of Tenochtitlán"
      [Ancient, Tenochtitlan]
      Diamond
      [Heart, Square]
      TheBoundaryBeyond

lakeXochimilco_182 :: CardDef
lakeXochimilco_182 =
  singleSided $
    location
      "04182"
      "Lake Xochimilco"
      [Ancient, Tenochtitlan]
      Heart
      [Diamond, Star]
      TheBoundaryBeyond

lakeXochimilco_183 :: CardDef
lakeXochimilco_183 =
  singleSided $
    location
      "04183"
      "Lake Xochimilco"
      [Ancient, Tenochtitlan]
      Heart
      [Diamond, Star]
      TheBoundaryBeyond

sacredWoods_184 :: CardDef
sacredWoods_184 =
  singleSided $
    location
      "04184"
      "Sacred Woods"
      [Ancient, Tenochtitlan]
      Star
      [Heart, Triangle]
      TheBoundaryBeyond

sacredWoods_185 :: CardDef
sacredWoods_185 =
  singleSided $
    location
      "04185"
      "Sacred Woods"
      [Ancient, Tenochtitlan]
      Star
      [Heart, Triangle]
      TheBoundaryBeyond

mouthOfKnYanTheCavernsMaw :: CardDef
mouthOfKnYanTheCavernsMaw =
  singleSided $
    location
      "04206"
      ("Mouth of K'n-yan" <:> "The Cavern's Maw")
      [Cave]
      Equals
      [Squiggle, T, Hourglass]
      HeartOfTheElders

mouthOfKnYanTheDepthsBelow :: CardDef
mouthOfKnYanTheDepthsBelow =
  singleSided $
    location
      "04206b"
      ("Mouth of K'n-yan" <:> "The Depths Below")
      [Cave]
      Equals
      [Circle, Triangle, Diamond]
      HeartOfTheElders

timeWrackedWoods :: CardDef
timeWrackedWoods =
  victory 1 $
    singleSided $
      location
        "04217"
        "Time-Wracked Woods"
        [Jungle]
        Circle
        [Square, Diamond, Moon]
        PillarsOfJudgement

stoneAltar :: CardDef
stoneAltar =
  victory 1 $
    singleSided $
      location
        "04218"
        "Stone Altar"
        [Ancient, Ruins]
        Hourglass
        [Triangle, Heart, Equals]
        PillarsOfJudgement

vastPassages :: CardDef
vastPassages =
  singleSided $
    location
      "04222"
      "Vast Passages"
      [Ancient, Cave]
      Circle
      [Equals, Triangle, Diamond, Square, Moon]
      KnYan

hallOfIdolatry :: CardDef
hallOfIdolatry =
  victory 1 $
    singleSided $
      location
        "04223"
        "Hall of Idolatry"
        [Ancient, Cave]
        Square
        [Heart, Triangle, Circle]
        KnYan

darkHollow :: CardDef
darkHollow =
  victory 1 $
    singleSided $
      location
        "04224"
        "Dark Hollow"
        [Ancient, Cave]
        Triangle
        [Equals, Circle, Square]
        KnYan

perilousGulch :: CardDef
perilousGulch =
  victory 1 $
    singleSided $
      location
        "04225"
        "Perilous Gulch"
        [Ancient, Cave]
        Diamond
        [Equals, Circle, Moon]
        KnYan

crystalPillars :: CardDef
crystalPillars =
  victory 1 $
    singleSided $
      location
        "04226"
        "Crystal Pillars"
        [Ancient, Cave]
        Moon
        [Heart, Diamond, Circle]
        KnYan

descentToYoth :: CardDef
descentToYoth =
  victory 2 $
    vengeance 2 $
      singleSided $
        location
          "04227"
          "Descent to Yoth"
          [Ancient, Cave]
          Heart
          [Square, Moon]
          KnYan

interviewRoomArrivalChamber :: CardDef
interviewRoomArrivalChamber =
  locationWithUnrevealed
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

interviewRoomRestrainingChamber :: CardDef
interviewRoomRestrainingChamber =
  victory 1 $
    locationWithUnrevealed
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

interviewRoomIchorFilledChamber :: CardDef
interviewRoomIchorFilledChamber =
  victory 1 $
    locationWithUnrevealed
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

hallsOfPnakotusNorthernCorridors :: CardDef
hallsOfPnakotusNorthernCorridors =
  location
    "04248"
    ("Halls of Pnakotus" <:> "Northern Corridors")
    [Ancient, Pnakotus]
    Squiggle
    [Diamond, Square, Triangle]
    TheCityOfArchives

hallsOfPnakotusEasternCorridors :: CardDef
hallsOfPnakotusEasternCorridors =
  location
    "04249"
    ("Halls of Pnakotus" <:> "Eastern Corridors")
    [Ancient, Pnakotus]
    Diamond
    [Squiggle, Square, Droplet]
    TheCityOfArchives

hallsOfPnakotusWesternCorridors :: CardDef
hallsOfPnakotusWesternCorridors =
  location
    "04250"
    ("Halls of Pnakotus" <:> "Western Corridors")
    [Ancient, Pnakotus]
    Square
    [Squiggle, Diamond, Circle, Star]
    TheCityOfArchives

greatLibrary :: CardDef
greatLibrary =
  location
    "04251"
    "Great Library"
    [Ancient, Pnakotus]
    Circle
    [Square]
    TheCityOfArchives

yithianOrrery :: CardDef
yithianOrrery =
  victory 1 $
    location
      "04252"
      "Yithian Orrery"
      [Ancient, Pnakotus]
      Moon
      [Triangle]
      TheCityOfArchives

laboratoryOfTheGreatRace :: CardDef
laboratoryOfTheGreatRace =
  location
    "04253"
    "Laboratory of the Great Race"
    [Ancient, Pnakotus]
    Triangle
    [Squiggle, Moon, Equals]
    TheCityOfArchives

deconstructionRoom :: CardDef
deconstructionRoom =
  victory 1 $
    location
      "04254"
      "Deconstruction Room"
      [Ancient, Pnakotus]
      Equals
      [Triangle]
      TheCityOfArchives

towersOfPnakotus :: CardDef
towersOfPnakotus =
  victory 2 $
    location
      "04255"
      "Towers of Pnakotus"
      [Ancient, Pnakotus]
      Star
      [Square]
      TheCityOfArchives

stepsOfYoth :: CardDef
stepsOfYoth =
  singleSided $
    location
      "04286"
      "Steps of Yoth"
      [Ancient, Forgotten, Yoth]
      Equals
      [Hourglass, Square, Triangle, Diamond, Heart]
      TheDepthsOfYoth

cityOfTheSerpents :: CardDef
cityOfTheSerpents =
  vengeance 2 $
    singleSided $
      location
        "04287"
        "City of the Serpents"
        [Ancient, Cave, Yoth]
        Diamond
        [Equals, Droplet, Triangle, T, Square]
        TheDepthsOfYoth

hallOfHeresy :: CardDef
hallOfHeresy =
  vengeance 2 $
    singleSided $
      location
        "04288"
        "Hall of Heresy"
        [Ancient, Cave, Yoth]
        Triangle
        [Equals, Diamond, Circle, Square, T]
        TheDepthsOfYoth

crumblingPrecipice :: CardDef
crumblingPrecipice =
  singleSided $
    location
      "04289"
      "Crumbling Precipice"
      [Ancient, Cave, Yoth]
      Hourglass
      [Equals, Squiggle, Heart, T, Droplet]
      TheDepthsOfYoth

cavernsOfYoth :: CardDef
cavernsOfYoth =
  singleSided $
    location
      "04290"
      "Caverns of Yoth"
      [Ancient, Cave, Yoth]
      Droplet
      [Circle, Hourglass, Heart, Diamond, Squiggle]
      TheDepthsOfYoth

forkedPath :: CardDef
forkedPath =
  singleSided $
    location
      "04291"
      "Forked Path"
      [Ancient, Cave, Yoth]
      T
      [Circle, Diamond, Hourglass, Square, Triangle]
      TheDepthsOfYoth

bridgeOverNKai :: CardDef
bridgeOverNKai =
  singleSided $
    location
      "04292"
      "Bridge over N'kai"
      [Ancient, Cave, Yoth]
      Heart
      [Equals, Circle, Droplet, Hourglass, Squiggle]
      TheDepthsOfYoth

brokenPassage :: CardDef
brokenPassage =
  singleSided $
    location
      "04293"
      "Broken Passage"
      [Ancient, Cave, Yoth]
      Squiggle
      [Circle, Droplet, Hourglass, Square, Heart]
      TheDepthsOfYoth

abandonedSite :: CardDef
abandonedSite =
  singleSided $
    location
      "04294"
      "Abandoned Site"
      [Ancient, Cave, Yoth]
      Square
      [Equals, Diamond, Triangle, T, Squiggle]
      TheDepthsOfYoth

brightCanyon :: CardDef
brightCanyon =
  singleSided $
    location
      "04295"
      "Bright Canyon"
      [Ancient, Cave, Yoth]
      Circle
      [Droplet, Squiggle, T, Heart, Triangle]
      TheDepthsOfYoth

nexusOfNKai :: CardDef
nexusOfNKai =
  location
    "04324"
    ("Nexus of N'kai" <:> "Unraveling the Threads")
    [Ancient, Ruins]
    Diamond
    [Droplet, Star]
    ShatteredAeons

yuggoth :: CardDef
yuggoth =
  singleSided $
    location "04327" "Yuggoth" [Otherworld] Droplet [Diamond] ShatteredAeons

shoresOfRlyeh :: CardDef
shoresOfRlyeh =
  singleSided $
    location
      "04328"
      "Shores of R'lyeh"
      [Otherworld]
      Droplet
      [Diamond]
      ShatteredAeons

cityOfTheUnseen :: CardDef
cityOfTheUnseen =
  singleSided $
    location
      "04329"
      "City of the Unseen"
      [Otherworld]
      Droplet
      [Diamond]
      ShatteredAeons

aPocketInTime :: CardDef
aPocketInTime =
  victory 1 $
    singleSided $
      location
        "04330"
        "A Pocket in Time"
        [Extradimensional]
        Star
        [Diamond, Equals]
        ShatteredAeons

ruinsOfNewYork :: CardDef
ruinsOfNewYork =
  singleSided $
    location
      "04331"
      "Ruins of New York"
      [Shattered, Future, Ruins]
      Equals
      [Star]
      ShatteredAeons

mu :: CardDef
mu =
  victory 1 $
    singleSided $
      location
        "04332"
        "Mu"
        [Shattered, Ancient]
        Equals
        [Star]
        ShatteredAeons

atlantis :: CardDef
atlantis =
  singleSided $
    location
      "04333"
      "Atlantis"
      [Shattered, Ancient]
      Equals
      [Star]
      ShatteredAeons

pnakotus :: CardDef
pnakotus =
  victory 1 $
    singleSided $
      location
        "04334"
        "Pnakotus"
        [Shattered, Ancient]
        Equals
        [Star]
        ShatteredAeons

valusia :: CardDef
valusia =
  victory 1 $
    singleSided $
      location
        "04335"
        "Valusia"
        [Shattered, Ancient]
        Equals
        [Star]
        ShatteredAeons

plateauOfLeng :: CardDef
plateauOfLeng =
  singleSided $
    location
      "04336"
      "Plateau of Leng"
      [Shattered, PresentDay]
      Equals
      [Star]
      ShatteredAeons

witchesCircle :: CardDef
witchesCircle =
  revelation $
    victory 2 $
      singleSided $
        location
          "05055b"
          "Witches' Circle"
          [Woods, Trait.Circle]
          Plus
          [Squiggle]
          TheWitchingHour

witchHauntedWoodsAbandonedMine :: CardDef
witchHauntedWoodsAbandonedMine =
  victory 1 $
    locationWithUnrevealed
      "05058"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Abandoned Mine")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsCairnStones :: CardDef
witchHauntedWoodsCairnStones =
  victory 1 $
    locationWithUnrevealed
      "05059"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Cairn Stones")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsTheLonelyTree :: CardDef
witchHauntedWoodsTheLonelyTree =
  victory 1 $
    locationWithUnrevealed
      "05060"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "The Lonely Tree")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsChildsTreeHouse :: CardDef
witchHauntedWoodsChildsTreeHouse =
  victory 1 $
    locationWithUnrevealed
      "05061"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Child's Tree House")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsTaintedWell :: CardDef
witchHauntedWoodsTaintedWell =
  victory 1 $
    locationWithUnrevealed
      "05062"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Tainted Well")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsHermitsHouse :: CardDef
witchHauntedWoodsHermitsHouse =
  victory 1 $
    locationWithUnrevealed
      "05063"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Hermit's House")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsOvergrownBarn :: CardDef
witchHauntedWoodsOvergrownBarn =
  victory 1 $
    locationWithUnrevealed
      "05064"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Overgrown Barn")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

entryHallAtDeathsDoorstep :: CardDef
entryHallAtDeathsDoorstep =
  location
    "05071"
    "Entry Hall"
    []
    Square
    [T]
    AtDeathsDoorstep

victorianHalls :: CardDef
victorianHalls =
  location
    "05072"
    "Victorian Halls"
    []
    T
    [Square, Star, Triangle, Heart]
    AtDeathsDoorstep

trophyRoom :: CardDef
trophyRoom =
  location
    "05073"
    "Trophy Room"
    []
    Triangle
    [T, Diamond]
    AtDeathsDoorstep

billiardsRoom :: CardDef
billiardsRoom =
  location
    "05074"
    "Billiards Room"
    []
    Diamond
    [Triangle]
    AtDeathsDoorstep

masterBedroom :: CardDef
masterBedroom =
  location
    "05075"
    "Master Bedroom"
    []
    Heart
    [T, Moon]
    AtDeathsDoorstep

balconyAtDeathsDoorstep :: CardDef
balconyAtDeathsDoorstep =
  location
    "05076"
    "Balcony"
    []
    Moon
    [Heart]
    AtDeathsDoorstep

office :: CardDef
office =
  location
    "05077"
    "Office"
    []
    Star
    [T]
    AtDeathsDoorstep

entryHallSpectral :: CardDef
entryHallSpectral =
  location
    "05078"
    "Entry Hall"
    [Spectral]
    Square
    [T]
    AtDeathsDoorstep

victorianHallsSpectral :: CardDef
victorianHallsSpectral =
  location
    "05079"
    "Victorian Halls"
    [Spectral]
    T
    [Square, Star, Triangle, Heart]
    AtDeathsDoorstep

trophyRoomSpectral :: CardDef
trophyRoomSpectral =
  location
    "05080"
    "Trophy Room"
    [Spectral]
    Triangle
    [T, Diamond]
    AtDeathsDoorstep

billiardsRoomSpectral :: CardDef
billiardsRoomSpectral =
  victory 1 $
    location
      "05081"
      "Billiards Room"
      [Spectral]
      Diamond
      [Triangle]
      AtDeathsDoorstep

masterBedroomSpectral :: CardDef
masterBedroomSpectral =
  victory 1 $
    location
      "05082"
      "Master Bedroom"
      [Spectral]
      Heart
      [T, Moon]
      AtDeathsDoorstep

balconySpectral :: CardDef
balconySpectral =
  location
    "05083"
    "Balcony"
    [Spectral]
    Moon
    [Heart]
    AtDeathsDoorstep

officeSpectral :: CardDef
officeSpectral =
  victory 2 $
    location
      "05084"
      "Office"
      [Spectral]
      Star
      [T]
      AtDeathsDoorstep

moldyHalls :: CardDef
moldyHalls =
  location
    "05128"
    "Moldy Halls"
    [WitchHouse]
    Triangle
    [Plus, Circle, Heart, Square, Triangle]
    TheSecretName

landlordsQuarters :: CardDef
landlordsQuarters =
  locationWithUnrevealed
    "05129"
    "Decrepit Door"
    [WitchHouse]
    Plus
    [Triangle]
    "Landlord's Quarters"
    [WitchHouse]
    Circle
    [Triangle]
    TheSecretName

joeMazurewiczsRoom :: CardDef
joeMazurewiczsRoom =
  locationWithUnrevealed
    "05130"
    "Decrepit Door"
    [WitchHouse]
    Plus
    [Triangle]
    "Joe Mazurewicz's Room"
    [WitchHouse]
    Heart
    [Triangle]
    TheSecretName

frankElwoodsRoom :: CardDef
frankElwoodsRoom =
  locationWithUnrevealed
    "05131"
    "Decrepit Door"
    [WitchHouse]
    Plus
    [Triangle]
    "Frank Elwood's room"
    [WitchHouse]
    Diamond
    [Triangle]
    TheSecretName

walterGilmansRoom :: CardDef
walterGilmansRoom =
  location
    "05132"
    "Walter Gilman's Room"
    [WitchHouse]
    Square
    [Triangle]
    TheSecretName

keziahsRoom :: CardDef
keziahsRoom =
  location
    "05133"
    "Keziah's Room"
    [Spectral, WitchHouse]
    Square
    [Moon, Hourglass, T, Equals, Squiggle]
    TheSecretName

moldyHallsEarlierTonight :: CardDef
moldyHallsEarlierTonight =
  locationWithUnrevealed
    "05134"
    "Unknown Places"
    [Extradimensional]
    Moon
    [Square]
    ("Moldy Halls" <:> "Earlier Tonight")
    [Extradimensional, WitchHouse]
    Moon
    [Square]
    TheSecretName

twilightAbyss :: CardDef
twilightAbyss =
  victory 1 $
    locationWithUnrevealed
      "05135"
      "Unknown Places"
      [Extradimensional]
      Moon
      [Square]
      "Twilight Abyss"
      [Extradimensional, Otherworld]
      Equals
      [Square, Squiggle]
      TheSecretName

cityOfElderThings :: CardDef
cityOfElderThings =
  victory 1 $
    locationWithUnrevealed
      "05136"
      "Unknown Places"
      [Extradimensional]
      Moon
      [Square]
      "City of Elder Things"
      [Extradimensional, Otherworld]
      Moon
      [Square]
      TheSecretName

witchHouseRuins :: CardDef
witchHouseRuins =
  locationWithUnrevealed
    "05137"
    "Unknown Places"
    [Extradimensional]
    Moon
    [Square]
    "Witch House Ruins"
    [Extradimensional, WitchHouse]
    Hourglass
    [Square, T]
    TheSecretName

salemGaol1692 :: CardDef
salemGaol1692 =
  locationWithUnrevealed
    "05138"
    "Unknown Places"
    [Extradimensional]
    Moon
    [Square]
    "Salem Gaol, 1692"
    [Extradimensional, Salem]
    Moon
    [Square]
    TheSecretName

physicsClassroom :: CardDef
physicsClassroom =
  victory 1 $
    locationWithUnrevealed
      "05139"
      "Unknown Places"
      [Extradimensional]
      Moon
      [Square]
      "Physics Classroom"
      [Extradimensional, Miskatonic]
      Moon
      [Square]
      TheSecretName

courtOfTheGreatOldOnes :: CardDef
courtOfTheGreatOldOnes =
  victory 1 $
    locationWithUnrevealed
      "05140"
      "Unknown Places"
      [Extradimensional]
      Moon
      [Square]
      ("Court of the Great Old Ones" <:> "A Not-Too-Distant Future")
      [Extradimensional, Otherworld]
      Squiggle
      [Square, Equals]
      TheSecretName

siteOfTheSacrifice :: CardDef
siteOfTheSacrifice =
  location
    "05141"
    "Site of the Sacrifice"
    [Extradimensional, WitchHouse]
    T
    [Hourglass]
    TheSecretName

strangeGeometry :: CardDef
strangeGeometry =
  singleSided $
    location
      "05142"
      "Strange Geometry"
      [Extradimensional]
      NoSymbol
      []
      TheSecretName

hangmansBrook :: CardDef
hangmansBrook =
  singleSided $
    location
      "05166"
      "Hangman's Brook"
      mempty
      Squiggle
      [Circle, Plus]
      TheWagesOfSin

hangmansBrookSpectral :: CardDef
hangmansBrookSpectral =
  singleSided $
    location
      "05166b"
      "Hangman's Brook"
      [Spectral]
      Squiggle
      [Circle, Plus]
      TheWagesOfSin

hauntedFields :: CardDef
hauntedFields =
  victory 1 $
    singleSided $
      location
        "05167"
        "Haunted Fields"
        mempty
        Circle
        [Squiggle, Plus, Triangle, Square]
        TheWagesOfSin

hauntedFieldsSpectral :: CardDef
hauntedFieldsSpectral =
  victory 1 $
    singleSided $
      location
        "05167b"
        "Haunted Fields"
        [Spectral]
        Circle
        [Squiggle, Plus, Diamond, Square]
        TheWagesOfSin

abandonedChapel :: CardDef
abandonedChapel =
  victory 1 $
    singleSided $
      location
        "05168"
        "Abandoned Chapel"
        mempty
        Plus
        [Squiggle, Circle, Diamond, Moon]
        TheWagesOfSin

abandonedChapelSpectral :: CardDef
abandonedChapelSpectral =
  victory 1 $
    singleSided $
      location
        "05168b"
        "Abandoned Chapel"
        [Spectral]
        Plus
        [Squiggle, Circle, Diamond, Moon]
        TheWagesOfSin

theGallows_169 :: CardDef
theGallows_169 =
  singleSided $
    location
      "05169"
      "The Gallows"
      mempty
      Triangle
      [Circle, Square]
      TheWagesOfSin

theGallowsSpectral_169 :: CardDef
theGallowsSpectral_169 =
  singleSided $
    location
      "05169b"
      "The Gallows"
      [Spectral]
      Triangle
      [Circle, Square]
      TheWagesOfSin

theGallows_170 :: CardDef
theGallows_170 =
  singleSided $
    location
      "05170"
      "The Gallows"
      mempty
      Triangle
      [Circle, Square]
      TheWagesOfSin

theGallowsSpectral_170 :: CardDef
theGallowsSpectral_170 =
  singleSided $
    location
      "05170b"
      "The Gallows"
      [Spectral]
      Triangle
      [Circle, Square]
      TheWagesOfSin

hereticsGraves_171 :: CardDef
hereticsGraves_171 =
  singleSided $
    location
      "05171"
      "Heretics' Graves"
      mempty
      Square
      [Triangle, Circle]
      TheWagesOfSin

hereticsGravesSpectral_171 :: CardDef
hereticsGravesSpectral_171 =
  singleSided $
    location
      "05171b"
      "Heretics' Graves"
      [Spectral]
      Square
      [Triangle, Circle]
      TheWagesOfSin

hereticsGraves_172 :: CardDef
hereticsGraves_172 =
  singleSided $
    location
      "05172"
      "Heretics' Graves"
      mempty
      Square
      [Triangle, Circle]
      TheWagesOfSin

hereticsGravesSpectral_172 :: CardDef
hereticsGravesSpectral_172 =
  singleSided $
    location
      "05172b"
      "Heretics' Graves"
      [Spectral]
      Square
      [Triangle, Circle]
      TheWagesOfSin

chapelCrypt_173 :: CardDef
chapelCrypt_173 =
  singleSided $
    location
      "05173"
      "Chapel Crypt"
      mempty
      Diamond
      [Plus, Moon]
      TheWagesOfSin

chapelCryptSpectral_173 :: CardDef
chapelCryptSpectral_173 =
  singleSided $
    location
      "05173b"
      "Chapel Crypt"
      [Spectral]
      Diamond
      [Plus, Moon]
      TheWagesOfSin

chapelCrypt_174 :: CardDef
chapelCrypt_174 =
  singleSided $
    location
      "05174"
      "Chapel Crypt"
      mempty
      Diamond
      [Plus, Moon]
      TheWagesOfSin

chapelCryptSpectral_174 :: CardDef
chapelCryptSpectral_174 =
  singleSided $
    location
      "05174b"
      "Chapel Crypt"
      [Spectral]
      Diamond
      [Plus, Moon]
      TheWagesOfSin

chapelAttic_175 :: CardDef
chapelAttic_175 =
  singleSided $
    location
      "05175"
      "Chapel Attic"
      mempty
      Moon
      [Plus, Diamond]
      TheWagesOfSin

chapelAtticSpectral_175 :: CardDef
chapelAtticSpectral_175 =
  singleSided $
    location
      "05175b"
      "Chapel Attic"
      [Spectral]
      Moon
      [Plus, Diamond]
      TheWagesOfSin

chapelAttic_176 :: CardDef
chapelAttic_176 =
  singleSided $
    location
      "05176"
      "Chapel Attic"
      mempty
      Moon
      [Plus, Diamond]
      TheWagesOfSin

chapelAtticSpectral_176 :: CardDef
chapelAtticSpectral_176 =
  singleSided $
    location
      "05176b"
      "Chapel Attic"
      [Spectral]
      Moon
      [Plus, Diamond]
      TheWagesOfSin

lodgeGatesWeveBeenExpectingYou :: CardDef
lodgeGatesWeveBeenExpectingYou =
  location
    "05204"
    ("Lodge Gates" <:> "We've Been Expecting You")
    [Lodge]
    Diamond
    [Circle]
    ForTheGreaterGood

lodgeGatesMembersOnly :: CardDef
lodgeGatesMembersOnly =
  location
    "05205"
    ("Lodge Gates" <:> "Members Only")
    [Lodge]
    Diamond
    [T]
    ForTheGreaterGood

lobbyWeveBeenExpectingYou :: CardDef
lobbyWeveBeenExpectingYou =
  location
    "05206"
    ("Lobby" <:> "We've Been Expecting You")
    [Lodge]
    Circle
    [Diamond, T, Moon]
    ForTheGreaterGood

lobbyMembersOnly :: CardDef
lobbyMembersOnly =
  location
    "05207"
    ("Lobby" <:> "Members Only")
    [Lodge]
    Circle
    [T, Moon]
    ForTheGreaterGood

lodgeCellarWeveBeenExpectingYou :: CardDef
lodgeCellarWeveBeenExpectingYou =
  location
    "05208"
    ("Lodge Cellar" <:> "We've Been Expecting You")
    [Lodge]
    T
    [Circle, Squiggle]
    ForTheGreaterGood

lodgeCellarMembersOnly :: CardDef
lodgeCellarMembersOnly =
  location
    "05209"
    ("Lodge Cellar" <:> "Members Only")
    [Lodge]
    T
    [Diamond, Circle, Squiggle]
    ForTheGreaterGood

lounge :: CardDef
lounge =
  location
    "05210"
    "Lounge"
    [Lodge]
    Moon
    [Circle, Heart, Plus]
    ForTheGreaterGood

vault :: CardDef
vault =
  location
    "05211"
    "Vault"
    [Lodge]
    Plus
    [Moon]
    ForTheGreaterGood

library :: CardDef
library =
  location
    "05212"
    "Library"
    [Lodge]
    Heart
    [Moon]
    ForTheGreaterGood

lodgeCatacombs :: CardDef
lodgeCatacombs =
  location
    "05213"
    "Lodge Catacombs"
    [Lodge, Sanctum]
    Squiggle
    [T, Star, Triangle, Square, Hourglass]
    ForTheGreaterGood

sanctumDoorwayCeremonyRoom :: CardDef
sanctumDoorwayCeremonyRoom =
  locationWithUnrevealed
    "05214"
    "Sanctum Doorway"
    [Lodge, Sanctum]
    Star
    [Squiggle]
    "Ceremony Room"
    [Lodge, Sanctum]
    Triangle
    [Squiggle]
    ForTheGreaterGood

sanctumDoorwayHoldingCells :: CardDef
sanctumDoorwayHoldingCells =
  locationWithUnrevealed
    "05215"
    "Sanctum Doorway"
    [Lodge, Sanctum]
    Star
    [Squiggle]
    "Holding Cells"
    [Lodge, Sanctum]
    Square
    [Squiggle]
    ForTheGreaterGood

innerSanctum :: CardDef
innerSanctum =
  locationWithUnrevealed
    "05216"
    "Inner Sanctum"
    [Lodge, Sanctum]
    Star
    [Squiggle]
    "Inner Sanctum"
    [Lodge, Sanctum]
    Hourglass
    [Squiggle]
    ForTheGreaterGood

miskatonicRiver :: CardDef
miskatonicRiver =
  location
    "05249"
    "Miskatonic River"
    [River]
    Triangle
    [Moon]
    UnionAndDisillusion

forbiddingShore :: CardDef
forbiddingShore =
  location
    "05250"
    "Forbidding Shore"
    [Woods]
    Moon
    [Triangle, Squiggle]
    UnionAndDisillusion

unvisitedIsleStandingStones :: CardDef
unvisitedIsleStandingStones =
  victory 1 $
    locationWithUnrevealed
      "05251"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Standing Stones")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion

unvisitedIsleMistyClearing :: CardDef
unvisitedIsleMistyClearing =
  victory 1 $
    locationWithUnrevealed
      "05252"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Misty Clearing")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion

unvisitedIsleForsakenWoods :: CardDef
unvisitedIsleForsakenWoods =
  victory 1 $
    locationWithUnrevealed
      "05253"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Forsaken Woods")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion

unvisitedIsleMossCoveredSteps :: CardDef
unvisitedIsleMossCoveredSteps =
  victory 1 $
    locationWithUnrevealed
      "05254"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Moss-Covered Steps")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion

unvisitedIsleHauntedSpring :: CardDef
unvisitedIsleHauntedSpring =
  victory 1 $
    locationWithUnrevealed
      "05255"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Haunted Spring")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion

unvisitedIsleDecayedWillow :: CardDef
unvisitedIsleDecayedWillow =
  victory 1 $
    locationWithUnrevealed
      "05256"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Decayed Willow")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion

theGeistTrap :: CardDef
theGeistTrap =
  victory 1 $
    location
      "05257"
      "The Geist Trap"
      [Woods, Spectral]
      Plus
      [Squiggle]
      UnionAndDisillusion

frenchHill_290 :: CardDef
frenchHill_290 =
  location
    "05290"
    "French Hill"
    [Arkham]
    T
    [Circle, Square, Star]
    InTheClutchesOfChaos

frenchHill_291 :: CardDef
frenchHill_291 =
  location
    "05291"
    "French Hill"
    [Arkham]
    T
    [Circle, Square, Star]
    InTheClutchesOfChaos

rivertown_292 :: CardDef
rivertown_292 =
  location
    "05292"
    "Rivertown"
    [Arkham]
    Circle
    [Square, Triangle, T]
    InTheClutchesOfChaos

rivertown_293 :: CardDef
rivertown_293 =
  location
    "05293"
    "Rivertown"
    [Arkham]
    Circle
    [Square, Triangle, T]
    InTheClutchesOfChaos

southside_294 :: CardDef
southside_294 =
  location
    "05294"
    "Southside"
    [Arkham, Central]
    Square
    [Circle, Triangle, Plus, T, Diamond]
    InTheClutchesOfChaos

southside_295 :: CardDef
southside_295 =
  location
    "05295"
    "Southside"
    [Arkham, Central]
    Square
    [Circle, Triangle, Plus, T, Diamond]
    InTheClutchesOfChaos

uptown_296 :: CardDef
uptown_296 =
  location
    "05296"
    "Uptown"
    [Arkham]
    Plus
    [Square, Triangle, Moon]
    InTheClutchesOfChaos

uptown_297 :: CardDef
uptown_297 =
  location
    "05297"
    "Uptown"
    [Arkham]
    Plus
    [Square, Triangle, Moon]
    InTheClutchesOfChaos

southChurch_298 :: CardDef
southChurch_298 =
  location
    "05298"
    "South Church"
    [Arkham]
    Diamond
    [Square]
    InTheClutchesOfChaos

southChurch_299 :: CardDef
southChurch_299 =
  location
    "05299"
    "South Church"
    [Arkham]
    Diamond
    [Square]
    InTheClutchesOfChaos

merchantDistrict_300 :: CardDef
merchantDistrict_300 =
  location
    "05300"
    "Merchant District"
    [Arkham]
    Triangle
    [Circle, Square, Plus]
    InTheClutchesOfChaos

merchantDistrict_301 :: CardDef
merchantDistrict_301 =
  location
    "05301"
    "Merchant District"
    [Arkham]
    Triangle
    [Circle, Square, Plus]
    InTheClutchesOfChaos

studyAberrantGateway :: CardDef
studyAberrantGateway =
  location
    "50013"
    ("Study" <:> "Aberrant Gateway")
    mempty
    Circle
    [T]
    ReturnToTheGathering

guestHall :: CardDef
guestHall =
  location
    "50014"
    "Guest Hall"
    mempty
    T
    [Circle, Heart, Star, Square]
    ReturnToTheGathering

bedroom :: CardDef
bedroom = location "50015" "Bedroom" mempty Heart [T] ReturnToTheGathering

bathroom :: CardDef
bathroom = location "50016" "Bathroom" mempty Star [T] ReturnToTheGathering

holeInTheWall :: CardDef
holeInTheWall =
  locationWithUnrevealed
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

returnToAttic :: CardDef
returnToAttic =
  locationWithUnrevealed
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

farAboveYourHouse :: CardDef
farAboveYourHouse =
  victory 1 $
    locationWithUnrevealed
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

returnToCellar :: CardDef
returnToCellar =
  locationWithUnrevealed
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

deepBelowYourHouse :: CardDef
deepBelowYourHouse =
  victory 1 $
    locationWithUnrevealed
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

easttownArkhamPoliceStation :: CardDef
easttownArkhamPoliceStation =
  victory 1 $
    locationWithUnrevealed
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

northsideTrainStation :: CardDef
northsideTrainStation =
  locationWithUnrevealed
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

miskatonicUniversityMiskatonicMuseum :: CardDef
miskatonicUniversityMiskatonicMuseum =
  locationWithUnrevealed
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

rivertownAbandonedWarehouse :: CardDef
rivertownAbandonedWarehouse =
  locationWithUnrevealed
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

arkhamWoodsGreatWillow :: CardDef
arkhamWoodsGreatWillow =
  locationWithUnrevealed
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

arkhamWoodsLakeside :: CardDef
arkhamWoodsLakeside =
  locationWithUnrevealed
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

arkhamWoodsCorpseRiddenClearing :: CardDef
arkhamWoodsCorpseRiddenClearing =
  locationWithUnrevealed
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

arkhamWoodsWoodenBridge :: CardDef
arkhamWoodsWoodenBridge =
  locationWithUnrevealed
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

cursedShores :: CardDef
cursedShores =
  location
    "81007"
    "Cursed Shores"
    [NewOrleans, Bayou]
    Square
    [Plus, Triangle, Diamond, Hourglass]
    TheBayou

gardenDistrict :: CardDef
gardenDistrict =
  locationWithUnrevealed
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

broadmoor :: CardDef
broadmoor =
  victory 1 $
    locationWithUnrevealed
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

brackishWaters :: CardDef
brackishWaters =
  location
    "81010"
    "Brackish Waters"
    [Riverside, Bayou]
    Triangle
    [Squiggle, Square, Diamond, Hourglass]
    TheBayou

audubonPark :: CardDef
audubonPark =
  victory 1 $
    locationWithUnrevealed
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

faubourgMarigny :: CardDef
faubourgMarigny =
  locationWithUnrevealed
    "81012"
    "Riverside"
    [Riverside]
    Squiggle
    [Triangle, Squiggle]
    "Faubourg Marigny"
    [Riverside]
    Squiggle
    [Triangle, Squiggle]
    TheBayou

forgottenMarsh :: CardDef
forgottenMarsh =
  location
    "81013"
    "Forgotten Marsh"
    [Wilderness, Bayou]
    Diamond
    [Moon, Square, Triangle, Hourglass]
    TheBayou

trappersCabin :: CardDef
trappersCabin =
  locationWithUnrevealed
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

twistedUnderbrush :: CardDef
twistedUnderbrush =
  victory 1 $
    locationWithUnrevealed
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

foulSwamp :: CardDef
foulSwamp =
  location
    "81016"
    "Foul Swamp"
    [Unhallowed, Bayou]
    Hourglass
    [Equals, Square, Triangle, Diamond]
    TheBayou

ritualGrounds :: CardDef
ritualGrounds =
  victory 1 $
    locationWithUnrevealed
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

overgrownCairns :: CardDef
overgrownCairns =
  locationWithUnrevealed
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

gondola :: CardDef
gondola =
  location "82006b" "Gondola" [Venice, Boat] NoSymbol [] CarnevaleOfHorrors

sanMarcoBasilica :: CardDef
sanMarcoBasilica =
  location "82008" "San Marco Basilica" [Venice] NoSymbol [] CarnevaleOfHorrors

canalSide :: CardDef
canalSide =
  location "82009" "Canal-side" [Venice] NoSymbol [] CarnevaleOfHorrors

streetsOfVenice :: CardDef
streetsOfVenice =
  location "82010" "Streets of Venice" [Venice] NoSymbol [] CarnevaleOfHorrors

rialtoBridge :: CardDef
rialtoBridge =
  location
    "82011"
    "Rialto Bridge"
    [Venice, Bridge]
    NoSymbol
    []
    CarnevaleOfHorrors

venetianGarden :: CardDef
venetianGarden =
  location "82012" "Venetian Garden" [Venice] NoSymbol [] CarnevaleOfHorrors

bridgeOfSighs :: CardDef
bridgeOfSighs =
  location
    "82013"
    "Bridge of Sighs"
    [Venice, Bridge]
    NoSymbol
    []
    CarnevaleOfHorrors

floodedSquare :: CardDef
floodedSquare =
  location "82014" "Flooded Square" [Venice] NoSymbol [] CarnevaleOfHorrors

accademiaBridge :: CardDef
accademiaBridge =
  location
    "82015"
    "Accademia Bridge"
    [Venice, Bridge]
    NoSymbol
    []
    CarnevaleOfHorrors

theGuardian :: CardDef
theGuardian =
  location "82016" "The Guardian" [Venice] NoSymbol [] CarnevaleOfHorrors

betweenWorlds :: CardDef
betweenWorlds =
  location "xbetween" "Between Worlds" [Hex] NoSymbol [] ShatteredAeons
