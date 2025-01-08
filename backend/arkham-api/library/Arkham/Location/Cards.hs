module Arkham.Location.Cards where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet hiding (Dreamlands, Dunwich)
import Arkham.Keyword qualified as Keyword
import Arkham.LocationSymbol
import Arkham.Name
import Arkham.Trait hiding (Circle)
import Arkham.Trait qualified as Trait
import Data.Set qualified as Set

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
    , cdLevel = Nothing
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
  (emptyCardDef cardCode name LocationType)
    { cdRevealedName = Just name
    , cdCardTraits = setFromList traits
    , cdRevealedCardTraits = setFromList traits
    , cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Just 1
    , cdDoubleSided = True
    , cdLocationSymbol = Just locationSymbol
    , cdLocationRevealedSymbol = Just locationSymbol
    , cdLocationConnections = connectedLocationSymbols
    , cdLocationRevealedConnections = connectedLocationSymbols
    , cdLevel = Nothing
    }

allLocationCards :: Map CardCode CardDef
allLocationCards =
  mapFromList
    $ map
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
      , airfield
      , alaskanWilds
      , alchemyLabs
      , altarToDagon
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
      , attic_AThousandShapesOfHorror
      , audubonPark
      , backAlley
      , backstage
      , baharna
      , balcony
      , balconyAtDeathsDoorstep
      , balconySpectral
      , ballroom
      , barrierCamp
      , baseCamp
      , baseOfTheHill
      , baseOfTheSteps
      , basement
      , basementHall
      , bathroom
      , bedroom
      , billiardsRoom
      , billiardsRoomSpectral
      , bishopsBrook_202
      , bishopsBrook_203
      , blackCave
      , blackReef
      , blastedHeath_248
      , blastedHeath_249
      , bleakPlainsBleakDesolation
      , bleakPlainsStarsOfAldebaran
      , blockedPassage
      , boneFilledCaverns
      , boneRiddenPit
      , bootleggersHideaway_174a
      , bootleggersHideaway_174b
      , boxOffice
      , brackishWaters
      , bridgeOfSighs
      , bridgeOverNKai
      , brightCanyon
      , broadSnowdrifts
      , broadmoor
      , brokenPassage
      , brokenSteps_289
      , brokenSteps_290
      , burialGround
      , burialPit
      , burnedRuins_204
      , burnedRuins_205
      , canalSaintMartin
      , canalSide
      , canalsOfTenochtitlan_180
      , canalsOfTenochtitlan_181
      , candlelitTunnels
      , cavernsBeneathTheMoonDarkSide
      , cavernsBeneathTheMoonLightSide
      , cavernsOfYoth
      , celephais
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
      , churningWaters
      , circuitousTrail
      , cityOfElderThings
      , cityOfGugs
      , cityOfTheMoonBeasts
      , cityOfTheSerpents
      , cityOfTheUnseen
      , cityWhichAppearsOnNoMap
      , cliffsideRoad_a
      , cliffsideRoad_b
      , cloister
      , cloverClubBar
      , cloverClubCardroom
      , cloverClubLounge
      , clutteredDormitory
      , coastalWaters
      , coldSpringGlen_244
      , coldSpringGlen_245
      , coldWastes
      , congregationalChurch_208
      , congregationalChurch_209
      , cosmicGate
      , cosmicIngress
      , courtyard
      , courtOfTheGreatOldOnes
      , courtOfTheGreatOldOnesANotTooDistantFuture
      , coyoacan
      , cragOfTheGhouls
      , crashSite
      , crumblingPrecipice
      , cryptOfTheSepulchralLamp
      , crystalPillars
      , crystallineCavern
      , curiositieShoppe
      , cursedShores
      , cyclopeanRuins_176a
      , cyclopeanRuins_176b
      , dancersMist
      , darkAbyss
      , darkHollow
      , darkSpires
      , darkenedHall
      , deckOfTheTheodosia
      , deconstructionRoom
      , deepBelowYourHouse
      , deepDrifts
      , deepOneGrotto_175a
      , deepOneGrotto_175b
      , deepOneNursery
      , depthsOfDemheTheHeightOfTheDepths
      , depthsOfDemheStepsOfThePalace
      , descentToYoth
      , desertedStation
      , desolateCoastline
      , desolateRoad_a
      , desolateRoad_b
      , destroyedPath
      , devilsHopYard_252
      , devilsHopYard_253
      , dimStreetsMappingTheStreets
      , dimStreetsTheArchway
      , dimStreetsTheKingsParade
      , dimensionalDoorway
      , dimensionalGap
      , dimlyLitRoad_a
      , dimlyLitRoad_b
      , dimlyLitRoad_c
      , diningCar
      , diningRoom
      , doorwayToTheDepths
      , dormitories
      , downstairsDoorwayDen
      , downstairsDoorwayParlor
      , downtownArkhamAsylum
      , downtownFirstBankOfArkham
      , drKenslersOffice
      , dreamGatePointlessReality
      , dreamGateWondrousJourney
      , dressingRoom
      , dunwichVillage_242
      , dunwichVillage_243
      , dyersClassroom
      , dylathLeen
      , easttown
      , easttownArkhamPoliceStation
      , eerieGlade
      , elderChamber
      , emergencyRoom
      , enchantedWoodsFungalForest
      , enchantedWoodsGreatStoneCircle
      , enchantedWoodsLostWoods
      , enchantedWoodsMysticalForest
      , enchantedWoodsStoneTrapdoor
      , enchantedWoodsTheMoonTree
      , enchantedWoodsVillageOfZoogs
      , endlessBridge
      , engineCar_175
      , engineCar_176
      , engineCar_177
      , entryHall
      , entryHallAtDeathsDoorstep
      , entryHallSpectral
      , entryway
      , esotericOrderOfDagon
      , esotericOrderOfDagonInTooDeep
      , exhibitHallAthabaskanExhibit
      , exhibitHallEgyptianExhibit
      , exhibitHallHallOfTheDead
      , exhibitHallMedusaExhibit
      , exhibitHallNatureExhibit
      , exhibitHallRestrictedHall
      , expeditionCamp
      , experimentalTherapiesWard
      , eztliExhibit
      , facultyOfficesTheHourIsLate
      , facultyOfficesTheNightIsStillYoung
      , falconPointApproach
      , falconPointCliffside
      , falconPointGatehouse
      , farAboveYourHouse
      , faubourgMarigny
      , firstNationalGrocery
      , firstNationalGroceryInTooDeep
      , fishGraveyard
      , fishStreetBridge
      , fishStreetBridgeInTooDeep
      , flightIntoOblivion
      , floodedSquare
      , forbiddenLands
      , forbiddingShore
      , forgottenMarsh
      , forkInTheRoad_a
      , forkInTheRoad_b
      , forkedPath
      , forsakenTowerOfEternalFlame
      , forsakenTowerOfIllusionAndMyth
      , forsakenTowerOfInfiniteTruth
      , forsakenTowerOfLifeAndDeath
      , forsakenTowerOfPrimevalLight
      , forsakenTowerOfTheQueenOfNight
      , foulCorridors
      , foulSwamp
      , foyer
      , foyerMurderAtTheExcelsiorHotel
      , frankElwoodsRoom
      , frenchHill_290
      , frenchHill_291
      , frigidCave
      , frontPorchEntryway
      , frozenShores
      , frozenSpring
      , gallery
      , garden
      , gardenDistrict
      , gardensOfLuxembourg
      , gareDOrsay
      , gatewayToYhanthlei
      , gilmanHouse
      , gilmanHouseInTooDeep
      , gondola
      , grandChamber
      , grandEntryway
      , grandGuignol
      , grandRue
      , graveyard
      , greatLibrary
      , greenRoom
      , guestHall
      , hallOfBlood
      , hallOfHeresy
      , hallOfIdolatry
      , hallOfLoyalty
      , hallOfRebirth
      , hallOfSilence
      , hallOfTheDeep
      , hallsOfPnakotusEasternCorridors
      , hallsOfPnakotusNorthernCorridors
      , hallsOfPnakotusWesternCorridors
      , hallway
      , hangmansBrook
      , hangmansBrookSpectral
      , hangmansHillShroudedInMystery
      , hangmansHillWhereItAllEnds
      , hauntedFields
      , hauntedFieldsSpectral
      , hazuthKleg
      , hedgeMaze
      , hereticsGravesSpectral_171
      , hereticsGravesSpectral_172
      , hereticsGraves_171
      , hereticsGraves_172
      , hiddenCove
      , hiddenLibrary
      , hideousPalace
      , historicalSocietyHistoricalLibrary_133
      , historicalSocietyHistoricalLibrary_136
      , historicalSocietyHistoricalMuseum_130
      , historicalSocietyHistoricalMuseum_132
      , historicalSocietyMeetingRoom
      , historicalSocietyPeabodysOffice
      , historicalSocietyReadingRoom
      , historicalSocietyRecordOffice_129
      , historicalSocietyRecordOffice_138
      , holdingCells
      , holeInTheWall
      , hotelRoof
      , houseInTheReeds_210
      , houseInTheReeds_211
      , humanitiesBuilding
      , icebreakerLanding
      , icyWastes
      , idolChamber
      , ilekVad
      , infinityOfDarkness
      , infirmary
      , infirmaryFatalMirage
      , innerSanctum
      , innsmouthHarbour
      , innsmouthHarbourInTooDeep
      , innsmouthJail
      , innsmouthJailInTooDeep
      , innsmouthSquare
      , innsmouthSquareInTooDeep
      , intersection_a
      , intersection_b
      , interviewRoomArrivalChamber
      , interviewRoomIchorFilledChamber
      , interviewRoomRestrainingChamber
      , joeMazurewiczsRoom
      , kadatheron
      , keziahsRoom
      , kitchen
      , knightsHall
      , laBellaLuna
      , laboratoryOfTheGreatRace
      , labyrinthOfBones
      , lairOfDagon
      , lairOfDagonIntoTheMaelstrom
      , lairOfHydra
      , lakeXochimilco_182
      , lakeXochimilco_183
      , landlordsQuarters
      , lanternRoom
      , leMarais217
      , leMarais218
      , library
      , lightSideOfTheMoon
      , lighthouseKeepersCottage
      , lighthouseStairwell
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
      , lonelyIsle
      , longWayAround
      , lostMemories
      , lounge
      , mainPath
      , marshRefinery
      , marshRefineryInTooDeep
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
      , moaiStatues
      , moldyHalls
      , moldyHallsEarlierTonight
      , monasteryOfLeng
      , montmartre209
      , montmartre210
      , montparnasse
      , moonBeastGalley
      , moonForest
      , morgue
      , mouthOfKnYanTheCavernsMaw
      , mouthOfKnYanTheDepthsBelow
      , mtNgranek
      , mu
      , museumEntrance
      , museumHalls
      , mysteriousStairs_183
      , mysteriousStairs_184
      , mysteriousStairs_185
      , mysteriousStairs_186
      , mysteriousStairs_187
      , mysteriousStairs_188
      , namelessRuins
      , narrowRidge
      , narrowShaft
      , newChurchGreen
      , newChurchGreenInTooDeep
      , nexusOfNKai
      , northside
      , northsideTrainStation
      , northTower_287
      , northTower_288
      , notreDame
      , office
      , officeMurderAtTheExcelsiorHotel
      , officeSpectral
      , onyxGates
      , onyxGuardians
      , operaGarnier212
      , operaGarnier213
      , operatingRoom
      , orneLibrary
      , osbornsGeneralStore_206
      , osbornsGeneralStore_207
      , ottomanFront
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
      , pathwayIntoVoid
      , patientConfinementDanielsCell
      , patientConfinementOccupiedCell
      , patientConfinementDrearyCell
      , patientConfinementFamiliarCell
      , peaksOfThok
      , pereLachaiseCemetery
      , perilousGulch
      , physicsClassroom
      , plainOfTheGhouls
      , plateauOfLeng
      , plateauOfLengWhereTheGodsDwell
      , pnakotus
      , porteDeLAvancee
      , precariousIceSheet
      , prismaticCascade
      , prisonOfMemories
      , privateRoom
      , pumpRoom
      , quietHalls_131
      , quietHalls_135
      , railroadStation
      , recordsOffice
      , rehearsalRoom
      , remnantsOfLakesCamp
      , restaurant
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
      , riverviewTheatre
      , rockyCrags
      , rockyLedge
      , room212
      , room225
      , room245
      , ropeBridge
      , ruinsOfCarcosaAMomentsRest
      , ruinsOfCarcosaInhabitantOfCarcosa
      , ruinsOfCarcosaTheCoffin
      , ruinsOfEztli
      , ruinsOfIb
      , ruinsOfNewYork
      , sacredWoods_184
      , sacredWoods_185
      , salemGaol1692
      , saltMarshes
      , sanMarcoBasilica
      , sanctumDoorwayCeremonyRoom
      , sanctumDoorwayHoldingCells
      , sarnath
      , sawboneAlley
      , sawboneAlleyInTooDeep
      , schoolhouse_212
      , schoolhouse_213
      , scienceBuilding
      , seaOfBones
      , seaOfPitch_262
      , seaOfPitch_263
      , seaOfPitch_264
      , seaOfPitch_265
      , sealedExit
      , secondFloorHall
      , secretPassage
      , securityOffice_128
      , securityOffice_129
      , sentinelPeak
      , serannian
      , serpentsHaven
      , sevenHundredSteps
      , seventySteps
      , shiveringPools
      , shoresOfHali
      , shoresOfRlyeh
      , shorewardSlums
      , shorewardSlumsInTooDeep
      , shrineToHydra
      , silverTwilightLodgeShroudedInMystery
      , silverTwilightLodgeWhereItAllEnds
      , siteOfTheSacrifice
      , skaiRiver
      , slaughteredWoods
      , sleepingCar
      , snowCoveredCrag
      , snowGraves
      , southChurch_298
      , southChurch_299
      , southsideHistoricalSociety
      , southsideMasBoardingHouse
      , southside_294
      , southside_295
      , stMarysHospital
      , stairwell
      , standingStones
      , statuesInTheDeep
      , steepIncline
      , stepsOfYhagharl
      , stepsOfYoth
      , stoneAltar
      , stoneArchways
      , strangeGeometry
      , streetsOfVenice
      , studentUnion
      , study
      , studyAberrantGateway
      , submergedTemple
      , suiteBalcony
      , sunkenArchives
      , sunkenGrottoFinalDepths
      , sunkenGrottoLowerDepths
      , sunkenGrottoUpperDepths
      , sunkenHalls
      , syzygyChamber
      , tearThroughSpace
      , tearThroughTime
      , templeOfTheFang
      , templeOfTheMoonLizard
      , templeOfTheUnion_177a
      , templeOfTheUnion_177b
      , templeOfUnattainableDesires
      , templeRuins
      , templesOfTenochtitlan_176
      , templesOfTenochtitlan_177
      , temploMayor_174
      , temploMayor_175
      , tenAcreMeadow_246
      , tenAcreMeadow_247
      , theBlackCore
      , theBlackStone
      , theBlackThrone
      , theCavernOfFlame
      , theDarkCrater
      , theEdgeOfTheUniverse
      , theEnchantedPath
      , theGallowsSpectral_169
      , theGallowsSpectral_170
      , theGallows_169
      , theGallows_170
      , theGateToHell
      , theGeistTrap
      , theGreatWebCosmicWeb
      , theGreatWebPrisonOfCocoons
      , theGreatWebTangledWeb
      , theGreatWebVastWeb
      , theGreatWebWebStairs
      , theGreatWebWebWovenIsland
      , theGuardian
      , theHiddenChamber
      , theHouseOnWaterStreet
      , theHouseOnWaterStreetInTooDeep
      , theLittleBookshop
      , theLittleBookshopInTooDeep
      , theMoonRoom
      , theOnyxCastle
      , theSummit
      , theWhiteShip
      , theatre
      , tidalPool
      , tightTurn_a
      , tightTurn_b
      , tightTurn_c
      , timeWrackedWoods
      , tombOfShadows
      , towerOfKoth
      , towersOfPnakotus
      , townHall
      , trainTracks
      , trapRoom
      , trappersCabin
      , treacherousPath
      , trophyRoom
      , trophyRoomSpectral
      , twilightAbyss
      , twistedUnderbrush
      , ulthar
      , undergroundRiver
      , undergroundRuins
      , underseaCorridors
      , underwaterCavern
      , unfamiliarChamber
      , universityHalls
      , unmarkedTomb
      , unvisitedIsleDecayedWillow
      , unvisitedIsleForsakenWoods
      , unvisitedIsleHauntedSpring
      , unvisitedIsleMistyClearing
      , unvisitedIsleMossCoveredSteps
      , unvisitedIsleStandingStones
      , uprootedWoods
      , upstairsDoorwayBedroom
      , upstairsDoorwayLibrary
      , upstairsHallway
      , uptown_296
      , uptown_297
      , valeOfPnath
      , valusia
      , vastPassages
      , vault
      , vaultOfRiches
      , vaultsOfZin
      , velmasDiner
      , venetianGarden
      , victorianHalls
      , victorianHallsSpectral
      , villageCommons
      , vipArea
      , waitingRoom
      , walterGilmansRoom
      , wavewornIsland
      , wellOfSouls
      , whateleyRuins_250
      , whateleyRuins_251
      , whiteBluff
      , windsweptPath
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
      , zulanThek
      ]

withMeta :: ToJSON a => (Text, a) -> CardDef -> CardDef
withMeta (k, v) def = def {cdMeta = insertMap k (toJSON v) def.meta}

allSpecialLocationCards :: Map CardCode CardDef
allSpecialLocationCards =
  mapFromList $ map (toCardCode &&& id) [betweenWorlds, emptySpace]

vengeance :: Int -> CardDef -> CardDef
vengeance n def = def {cdVengeancePoints = Just n}

victory :: Int -> CardDef -> CardDef
victory n def = def {cdVictoryPoints = Just n}

revelation :: CardDef -> CardDef
revelation def = def {cdRevelation = IsRevelation}

singleSided :: CardDef -> CardDef
singleSided def = def {cdDoubleSided = False}

veiled :: CardDef -> CardDef
veiled def = def {cdDoubleSided = False, cdKeywords = Set.insert Keyword.Veiled $ cdKeywords def}

storyOnBack :: CardDef -> CardDef
storyOnBack def = def {cdDoubleSided = False}

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
  victory 1
    $ location
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
  victory 1
    $ location
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
  victory 1
    $ location "01133" "Graveyard" [Arkham] Hourglass [Circle] TheMidnightMasks

northside :: CardDef
northside =
  victory 1
    $ location
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
  victory 1
    $ location
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
  victory 1
    $ location
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
  victory 1
    $ location
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ location "02175" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress

engineCar_176 :: CardDef
engineCar_176 =
  victory 1
    $ location "02176" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress

engineCar_177 :: CardDef
engineCar_177 =
  victory 1
    $ location "02177" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress

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
    [Plus, Squiggle, Circle]
    BloodOnTheAltar

schoolhouse_213 :: CardDef
schoolhouse_213 =
  location
    "02213"
    "Schoolhouse"
    [Dunwich]
    Moon
    [Plus, Squiggle, Circle]
    BloodOnTheAltar

theHiddenChamber :: CardDef
theHiddenChamber =
  victory 2
    $ singleSided
    $ location
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
  victory 2
    $ location
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
  singleSided
    $ ( location
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
  singleSided
    $ ( location
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
  singleSided
    $ ( location
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
  singleSided
    $ location
      "02327"
      "Steps of Y'hagharl"
      [Otherworld, Extradimensional]
      Plus
      [Diamond, Moon]
      LostInTimeAndSpace

dimensionalDoorway :: CardDef
dimensionalDoorway =
  singleSided
    $ location
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
  victory 1
    $ location "03051" "Balcony" mempty Square [Circle, Triangle] CurtainCall

backstage :: CardDef
backstage =
  location "03052" "Backstage" mempty Diamond [Circle, Moon] CurtainCall

lightingBox :: CardDef
lightingBox =
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 2
    $ location "03139" "Hidden Library" mempty NoSymbol [] EchoesOfThePast

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
  victory 1
    $ location
      "03173"
      "Mess Hall"
      [ArkhamAsylum]
      Triangle
      [Circle, Square]
      TheUnspeakableOath

infirmary :: CardDef
infirmary =
  victory 1
    $ location
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
  victory 1
    $ location
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
  victory 1
    $ location
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
  victory 1
    $ location
      "03215"
      "Père Lachaise Cemetery"
      [Paris]
      T
      [Equals, Moon]
      APhantomOfTruth

canalSaintMartin :: CardDef
canalSaintMartin =
  victory 1
    $ location
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
  victory 1
    $ location
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
  quantity 2
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  quantity 2
    $ locationWithUnrevealed
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
  quantity 2
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ location
      "03285"
      "Outer Wall"
      []
      Triangle
      [Squiggle, Diamond, Equals]
      BlackStarsRise

outerWall_286 :: CardDef
outerWall_286 =
  victory 1
    $ location
      "03286"
      "Outer Wall"
      []
      Triangle
      [Squiggle, Diamond, Equals]
      BlackStarsRise

northTower_287 :: CardDef
northTower_287 =
  victory 1
    $ location
      "03287"
      "North Tower"
      []
      Diamond
      [Squiggle, Triangle, Equals]
      BlackStarsRise

northTower_288 :: CardDef
northTower_288 =
  victory 1
    $ location
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
  victory 1
    $ location
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
  victory 2
    $ locationWithUnrevealed
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
  victory 2
    $ locationWithUnrevealed
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
  storyOnBack $ location "03325a" "Shores of Hali" [Otherworld] Circle [Square] DimCarcosa

bleakPlainsStarsOfAldebaran :: CardDef
bleakPlainsStarsOfAldebaran =
  storyOnBack
    $ location
      "03326a"
      "Bleak Plains"
      [Otherworld]
      Square
      [Circle, Triangle, Diamond]
      DimCarcosa

bleakPlainsBleakDesolation :: CardDef
bleakPlainsBleakDesolation =
  storyOnBack
    $ location
      "03326c"
      "Bleak Plains"
      [Otherworld]
      Square
      [Circle, Triangle, Diamond]
      DimCarcosa

ruinsOfCarcosaInhabitantOfCarcosa :: CardDef
ruinsOfCarcosaInhabitantOfCarcosa =
  storyOnBack
    $ location
      "03327a"
      "Ruins of Carcosa"
      [Otherworld]
      Triangle
      [Square, Equals, Star]
      DimCarcosa

ruinsOfCarcosaAMomentsRest :: CardDef
ruinsOfCarcosaAMomentsRest =
  storyOnBack
    $ location
      "03327c"
      "Ruins of Carcosa"
      [Otherworld]
      Triangle
      [Square, Equals, Star]
      DimCarcosa

ruinsOfCarcosaTheCoffin :: CardDef
ruinsOfCarcosaTheCoffin =
  storyOnBack
    $ location
      "03327e"
      "Ruins of Carcosa"
      [Otherworld]
      Triangle
      [Square, Equals, Star]
      DimCarcosa

dimStreetsMappingTheStreets :: CardDef
dimStreetsMappingTheStreets =
  storyOnBack
    $ location
      "03328a"
      "Dim Streets"
      [Otherworld]
      Diamond
      [Square, Equals, Star]
      DimCarcosa

dimStreetsTheKingsParade :: CardDef
dimStreetsTheKingsParade =
  storyOnBack
    $ location
      "03328c"
      "Dim Streets"
      [Otherworld]
      Diamond
      [Square, Equals, Star]
      DimCarcosa

dimStreetsTheArchway :: CardDef
dimStreetsTheArchway =
  storyOnBack
    $ location
      "03328e"
      "Dim Streets"
      [Otherworld]
      Diamond
      [Square, Equals, Star]
      DimCarcosa

depthsOfDemheTheHeightOfTheDepths :: CardDef
depthsOfDemheTheHeightOfTheDepths =
  storyOnBack
    $ location
      "03329a"
      "Depths of Demhe"
      [Otherworld]
      Equals
      [Moon, Triangle, Diamond]
      DimCarcosa

depthsOfDemheStepsOfThePalace :: CardDef
depthsOfDemheStepsOfThePalace =
  storyOnBack
    $ location
      "03329c"
      "Depths of Demhe"
      [Otherworld]
      Equals
      [Moon, Triangle, Diamond]
      DimCarcosa

darkSpires :: CardDef
darkSpires = storyOnBack $ location "03330" "Dark Spires" [Otherworld] Moon [Equals] DimCarcosa

palaceOfTheKing :: CardDef
palaceOfTheKing =
  storyOnBack $ location "03331" "Palace of the King" [Otherworld] Star [Triangle, Diamond] DimCarcosa

expeditionCamp :: CardDef
expeditionCamp =
  location "04050" "Expedition Camp" [Campsite, Jungle] Circle [Square, Diamond, Moon] TheUntamedWilds

ruinsOfEztli :: CardDef
ruinsOfEztli =
  victory 2
    $ singleSided
    $ location "04053" "Ruins of Eztli" [Ancient, Ruins] Hourglass [Triangle, Heart] TheUntamedWilds

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
  singleSided
    $ location
      "04063"
      "Ancient Hall"
      [Ancient, Ruins]
      Square
      [Circle, Star, Diamond]
      TheDoomOfEztli

grandChamber :: CardDef
grandChamber =
  victory 1
    $ singleSided
    $ location
      "04064"
      "Grand Chamber"
      [Ancient, Ruins]
      Star
      [Circle, Square, Triangle]
      TheDoomOfEztli

burialPit :: CardDef
burialPit =
  victory 1
    $ singleSided
    $ location
      "04065"
      "Burial Pit"
      [Ancient, Ruins]
      Triangle
      [Star, Diamond, Squiggle]
      TheDoomOfEztli

undergroundRuins :: CardDef
undergroundRuins =
  vengeance 1
    $ singleSided
    $ location
      "04066"
      "Underground Ruins"
      [Ancient, Ruins]
      Diamond
      [Square, Triangle, Squiggle]
      TheDoomOfEztli

secretPassage :: CardDef
secretPassage =
  victory 1
    $ singleSided
    $ location
      "04067"
      "Secret Passage"
      [Ancient, Ruins]
      Squiggle
      [Diamond, Triangle, Hourglass]
      TheDoomOfEztli

chamberOfTime :: CardDef
chamberOfTime =
  victory 2
    $ vengeance 2
    $ singleSided
    $ location
      "04068"
      "Chamber of Time"
      [Forgotten, Ruins]
      Hourglass
      [Squiggle]
      TheDoomOfEztli

pathOfThorns :: CardDef
pathOfThorns =
  singleSided
    $ location
      "04069"
      "Path of Thorns"
      [Jungle]
      Square
      [Circle, Diamond, Triangle, Squiggle]
      Rainforest

riverCanyon :: CardDef
riverCanyon =
  singleSided
    $ location
      "04070"
      "River Canyon"
      [Jungle]
      Diamond
      [Circle, Moon, Heart, Triangle, Square]
      Rainforest

ropeBridge :: CardDef
ropeBridge =
  singleSided
    $ location
      "04071"
      "Rope Bridge"
      [Jungle]
      Moon
      [Circle, Diamond, Heart, T]
      Rainforest

serpentsHaven :: CardDef
serpentsHaven =
  victory 1
    $ singleSided
    $ location
      "04072"
      "Serpent's Haven"
      [Jungle]
      Triangle
      [Squiggle, Square, Diamond, Hourglass]
      Rainforest

circuitousTrail :: CardDef
circuitousTrail =
  victory 1
    $ singleSided
    $ location
      "04073"
      "Circuitous Trail"
      [Jungle]
      Heart
      [Hourglass, Diamond, Moon, T]
      Rainforest

templeOfTheFang :: CardDef
templeOfTheFang =
  victory 2
    $ singleSided
    $ location
      "04074"
      "Temple of the Fang"
      [Ancient, Ruins]
      Squiggle
      [Square, Triangle, Equals]
      Rainforest

overgrownRuins :: CardDef
overgrownRuins =
  victory 2
    $ singleSided
    $ location
      "04075"
      "Overgrown Ruins"
      [Ancient, Ruins]
      T
      [Moon, Heart, Equals]
      Rainforest

eztliExhibit :: CardDef
eztliExhibit =
  victory 1
    $ singleSided
    $ location
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
  victory 1
    $ location "04142" "Curiositie Shoppe" [Arkham] NoSymbol [T] ThreadsOfFate

townHall :: CardDef
townHall =
  victory 1
    $ location "04143" "Town Hall" [Arkham] NoSymbol [Triangle] ThreadsOfFate

arkhamPoliceStation :: CardDef
arkhamPoliceStation =
  victory 1
    $ singleSided
    $ location
      "04126b"
      "Arkham Police Station"
      [Arkham]
      NoSymbol
      [Moon]
      ThreadsOfFate

trainTracks :: CardDef
trainTracks =
  singleSided
    $ location "04128b" "Train Tracks" [Arkham] NoSymbol [T] ThreadsOfFate

blackCave :: CardDef
blackCave =
  victory 1
    $ singleSided
    $ location
      "04133b"
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
  singleSided
    $ location
      "04174"
      "Templo Mayor"
      [Ancient, Tenochtitlan]
      Circle
      [Square, Triangle]
      TheBoundaryBeyond

temploMayor_175 :: CardDef
temploMayor_175 =
  singleSided
    $ location
      "04175"
      "Templo Mayor"
      [Ancient, Tenochtitlan]
      Circle
      [Square, Triangle]
      TheBoundaryBeyond

templesOfTenochtitlan_176 :: CardDef
templesOfTenochtitlan_176 =
  singleSided
    $ location
      "04176"
      "Temples of Tenochtitlán"
      [Ancient, Tenochtitlan]
      Square
      [Diamond, Circle]
      TheBoundaryBeyond

templesOfTenochtitlan_177 :: CardDef
templesOfTenochtitlan_177 =
  singleSided
    $ location
      "04177"
      "Temples of Tenochtitlán"
      [Ancient, Tenochtitlan]
      Square
      [Diamond, Circle]
      TheBoundaryBeyond

chapultepecHill_178 :: CardDef
chapultepecHill_178 =
  singleSided
    $ location
      "04178"
      "Chapultepec Hill"
      [Ancient, Tenochtitlan]
      Triangle
      [Star, Circle]
      TheBoundaryBeyond

chapultepecHill_179 :: CardDef
chapultepecHill_179 =
  singleSided
    $ location
      "04179"
      "Chapultepec Hill"
      [Ancient, Tenochtitlan]
      Triangle
      [Star, Circle]
      TheBoundaryBeyond

canalsOfTenochtitlan_180 :: CardDef
canalsOfTenochtitlan_180 =
  singleSided
    $ location
      "04180"
      "Canals of Tenochtitlán"
      [Ancient, Tenochtitlan]
      Diamond
      [Heart, Square]
      TheBoundaryBeyond

canalsOfTenochtitlan_181 :: CardDef
canalsOfTenochtitlan_181 =
  singleSided
    $ location
      "04181"
      "Canals of Tenochtitlán"
      [Ancient, Tenochtitlan]
      Diamond
      [Heart, Square]
      TheBoundaryBeyond

lakeXochimilco_182 :: CardDef
lakeXochimilco_182 =
  singleSided
    $ location
      "04182"
      "Lake Xochimilco"
      [Ancient, Tenochtitlan]
      Heart
      [Diamond, Star]
      TheBoundaryBeyond

lakeXochimilco_183 :: CardDef
lakeXochimilco_183 =
  singleSided
    $ location
      "04183"
      "Lake Xochimilco"
      [Ancient, Tenochtitlan]
      Heart
      [Diamond, Star]
      TheBoundaryBeyond

sacredWoods_184 :: CardDef
sacredWoods_184 =
  singleSided
    $ location
      "04184"
      "Sacred Woods"
      [Ancient, Tenochtitlan]
      Star
      [Heart, Triangle]
      TheBoundaryBeyond

sacredWoods_185 :: CardDef
sacredWoods_185 =
  singleSided
    $ location
      "04185"
      "Sacred Woods"
      [Ancient, Tenochtitlan]
      Star
      [Heart, Triangle]
      TheBoundaryBeyond

mouthOfKnYanTheCavernsMaw :: CardDef
mouthOfKnYanTheCavernsMaw =
  singleSided
    $ location
      "04206"
      ("Mouth of K'n-yan" <:> "The Cavern's Maw")
      [Cave]
      Equals
      [Squiggle, T, Hourglass]
      HeartOfTheElders

mouthOfKnYanTheDepthsBelow :: CardDef
mouthOfKnYanTheDepthsBelow =
  singleSided
    $ location
      "04206b"
      ("Mouth of K'n-yan" <:> "The Depths Below")
      [Cave]
      Equals
      [Circle, Triangle, Diamond]
      HeartOfTheElders

timeWrackedWoods :: CardDef
timeWrackedWoods =
  victory 1
    $ singleSided
    $ location
      "04217"
      "Time-Wracked Woods"
      [Jungle]
      Circle
      [Square, Diamond, Moon]
      PillarsOfJudgement

stoneAltar :: CardDef
stoneAltar =
  victory 1
    $ singleSided
    $ location
      "04218"
      "Stone Altar"
      [Ancient, Ruins]
      Hourglass
      [Triangle, Heart, Equals]
      PillarsOfJudgement

vastPassages :: CardDef
vastPassages =
  singleSided
    $ location
      "04222"
      "Vast Passages"
      [Ancient, Cave]
      Circle
      [Equals, Triangle, Diamond, Square, Moon]
      KnYan

hallOfIdolatry :: CardDef
hallOfIdolatry =
  victory 1
    $ singleSided
    $ location
      "04223"
      "Hall of Idolatry"
      [Ancient, Cave]
      Square
      [Heart, Triangle, Circle]
      KnYan

darkHollow :: CardDef
darkHollow =
  victory 1
    $ singleSided
    $ location
      "04224"
      "Dark Hollow"
      [Ancient, Cave]
      Triangle
      [Equals, Circle, Square]
      KnYan

perilousGulch :: CardDef
perilousGulch =
  victory 1
    $ singleSided
    $ location
      "04225"
      "Perilous Gulch"
      [Ancient, Cave]
      Diamond
      [Equals, Circle, Moon]
      KnYan

crystalPillars :: CardDef
crystalPillars =
  victory 1
    $ singleSided
    $ location
      "04226"
      "Crystal Pillars"
      [Ancient, Cave]
      Moon
      [Heart, Diamond, Circle]
      KnYan

descentToYoth :: CardDef
descentToYoth =
  victory 2
    $ vengeance 2
    $ singleSided
    $ location
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ location
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
  victory 1
    $ location
      "04254"
      "Deconstruction Room"
      [Ancient, Pnakotus]
      Equals
      [Triangle]
      TheCityOfArchives

towersOfPnakotus :: CardDef
towersOfPnakotus =
  victory 2
    $ location
      "04255"
      "Towers of Pnakotus"
      [Ancient, Pnakotus]
      Star
      [Square]
      TheCityOfArchives

stepsOfYoth :: CardDef
stepsOfYoth =
  singleSided
    $ location
      "04286"
      "Steps of Yoth"
      [Ancient, Forgotten, Yoth]
      Equals
      [Hourglass, Square, Triangle, Diamond, Heart]
      TheDepthsOfYoth

cityOfTheSerpents :: CardDef
cityOfTheSerpents =
  vengeance 2
    $ singleSided
    $ location
      "04287"
      "City of the Serpents"
      [Ancient, Cave, Yoth]
      Diamond
      [Equals, Droplet, Triangle, T, Square]
      TheDepthsOfYoth

hallOfHeresy :: CardDef
hallOfHeresy =
  vengeance 2
    $ singleSided
    $ location
      "04288"
      "Hall of Heresy"
      [Ancient, Cave, Yoth]
      Triangle
      [Equals, Diamond, Circle, Square, T]
      TheDepthsOfYoth

crumblingPrecipice :: CardDef
crumblingPrecipice =
  singleSided
    $ location
      "04289"
      "Crumbling Precipice"
      [Ancient, Cave, Yoth]
      Hourglass
      [Equals, Squiggle, Heart, T, Droplet]
      TheDepthsOfYoth

cavernsOfYoth :: CardDef
cavernsOfYoth =
  singleSided
    $ location
      "04290"
      "Caverns of Yoth"
      [Ancient, Cave, Yoth]
      Droplet
      [Circle, Hourglass, Heart, Diamond, Squiggle]
      TheDepthsOfYoth

forkedPath :: CardDef
forkedPath =
  singleSided
    $ location
      "04291"
      "Forked Path"
      [Ancient, Cave, Yoth]
      T
      [Circle, Diamond, Hourglass, Square, Triangle]
      TheDepthsOfYoth

bridgeOverNKai :: CardDef
bridgeOverNKai =
  singleSided
    $ location
      "04292"
      "Bridge over N'kai"
      [Ancient, Cave, Yoth]
      Heart
      [Equals, Circle, Droplet, Hourglass, Squiggle]
      TheDepthsOfYoth

brokenPassage :: CardDef
brokenPassage =
  singleSided
    $ location
      "04293"
      "Broken Passage"
      [Ancient, Cave, Yoth]
      Squiggle
      [Circle, Droplet, Hourglass, Square, Heart]
      TheDepthsOfYoth

abandonedSite :: CardDef
abandonedSite =
  singleSided
    $ location
      "04294"
      "Abandoned Site"
      [Ancient, Cave, Yoth]
      Square
      [Equals, Diamond, Triangle, T, Squiggle]
      TheDepthsOfYoth

brightCanyon :: CardDef
brightCanyon =
  singleSided
    $ location
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
  singleSided
    $ location "04327" "Yuggoth" [Otherworld] Droplet [Diamond] ShatteredAeons

shoresOfRlyeh :: CardDef
shoresOfRlyeh =
  singleSided
    $ location
      "04328"
      "Shores of R'lyeh"
      [Otherworld]
      Droplet
      [Diamond]
      ShatteredAeons

cityOfTheUnseen :: CardDef
cityOfTheUnseen =
  singleSided
    $ location
      "04329"
      "City of the Unseen"
      [Otherworld]
      Droplet
      [Diamond]
      ShatteredAeons

aPocketInTime :: CardDef
aPocketInTime =
  victory 1
    $ singleSided
    $ location
      "04330"
      "A Pocket in Time"
      [Extradimensional]
      Star
      [Diamond, Equals]
      ShatteredAeons

ruinsOfNewYork :: CardDef
ruinsOfNewYork =
  singleSided
    $ location
      "04331"
      "Ruins of New York"
      [Shattered, Future, Ruins]
      Equals
      [Star]
      ShatteredAeons

mu :: CardDef
mu =
  victory 1
    $ singleSided
    $ location
      "04332"
      "Mu"
      [Shattered, Ancient]
      Equals
      [Star]
      ShatteredAeons

atlantis :: CardDef
atlantis =
  singleSided
    $ location
      "04333"
      "Atlantis"
      [Shattered, Ancient]
      Equals
      [Star]
      ShatteredAeons

pnakotus :: CardDef
pnakotus =
  victory 1
    $ singleSided
    $ location
      "04334"
      "Pnakotus"
      [Shattered, Ancient]
      Equals
      [Star]
      ShatteredAeons

valusia :: CardDef
valusia =
  victory 1
    $ singleSided
    $ location
      "04335"
      "Valusia"
      [Shattered, Ancient]
      Equals
      [Star]
      ShatteredAeons

plateauOfLeng :: CardDef
plateauOfLeng =
  singleSided
    $ location
      "04336"
      "Plateau of Leng"
      [Shattered, PresentDay]
      Equals
      [Star]
      ShatteredAeons

witchesCircle :: CardDef
witchesCircle =
  revelation
    $ victory 2
    $ singleSided
    $ location
      "05055b"
      "Witches' Circle"
      [Woods, Trait.Circle]
      Plus
      [Squiggle]
      TheWitchingHour

witchHauntedWoodsAbandonedMine :: CardDef
witchHauntedWoodsAbandonedMine =
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ location
      "05081"
      "Billiards Room"
      [Spectral]
      Diamond
      [Triangle]
      AtDeathsDoorstep

masterBedroomSpectral :: CardDef
masterBedroomSpectral =
  victory 1
    $ location
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
  victory 2
    $ location
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
    [Plus, Circle, Heart, Square, Diamond]
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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

courtOfTheGreatOldOnesANotTooDistantFuture :: CardDef
courtOfTheGreatOldOnesANotTooDistantFuture =
  victory 1
    $ locationWithUnrevealed
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
  quantity 2
    $ singleSided
    $ location
      "05142"
      "Strange Geometry"
      [Extradimensional]
      NoSymbol
      []
      TheSecretName

hangmansBrook :: CardDef
hangmansBrook =
  singleSided
    $ location
      "05166"
      "Hangman's Brook"
      mempty
      Squiggle
      [Circle, Plus]
      TheWagesOfSin

hangmansBrookSpectral :: CardDef
hangmansBrookSpectral =
  singleSided
    $ location
      "05166b"
      "Hangman's Brook"
      [Spectral]
      Squiggle
      [Circle, Plus]
      TheWagesOfSin

hauntedFields :: CardDef
hauntedFields =
  victory 1
    $ singleSided
    $ location
      "05167"
      "Haunted Fields"
      mempty
      Circle
      [Squiggle, Plus, Triangle, Square]
      TheWagesOfSin

hauntedFieldsSpectral :: CardDef
hauntedFieldsSpectral =
  victory 1
    $ singleSided
    $ location
      "05167b"
      "Haunted Fields"
      [Spectral]
      Circle
      [Squiggle, Plus, Triangle, Square]
      TheWagesOfSin

abandonedChapel :: CardDef
abandonedChapel =
  victory 1
    $ singleSided
    $ location
      "05168"
      "Abandoned Chapel"
      mempty
      Plus
      [Squiggle, Circle, Diamond, Moon]
      TheWagesOfSin

abandonedChapelSpectral :: CardDef
abandonedChapelSpectral =
  victory 1
    $ singleSided
    $ location
      "05168b"
      "Abandoned Chapel"
      [Spectral]
      Plus
      [Squiggle, Circle, Diamond, Moon]
      TheWagesOfSin

theGallows_169 :: CardDef
theGallows_169 =
  singleSided
    $ location
      "05169"
      "The Gallows"
      mempty
      Triangle
      [Circle, Square]
      TheWagesOfSin

theGallowsSpectral_169 :: CardDef
theGallowsSpectral_169 =
  singleSided
    $ location
      "05169b"
      "The Gallows"
      [Spectral]
      Triangle
      [Circle, Square]
      TheWagesOfSin

theGallows_170 :: CardDef
theGallows_170 =
  singleSided
    $ location
      "05170"
      "The Gallows"
      mempty
      Triangle
      [Circle, Square]
      TheWagesOfSin

theGallowsSpectral_170 :: CardDef
theGallowsSpectral_170 =
  singleSided
    $ location
      "05170b"
      "The Gallows"
      [Spectral]
      Triangle
      [Circle, Square]
      TheWagesOfSin

hereticsGraves_171 :: CardDef
hereticsGraves_171 =
  singleSided
    $ location
      "05171"
      "Heretics' Graves"
      mempty
      Square
      [Triangle, Circle]
      TheWagesOfSin

hereticsGravesSpectral_171 :: CardDef
hereticsGravesSpectral_171 =
  singleSided
    $ location
      "05171b"
      "Heretics' Graves"
      [Spectral]
      Square
      [Triangle, Circle]
      TheWagesOfSin

hereticsGraves_172 :: CardDef
hereticsGraves_172 =
  singleSided
    $ location
      "05172"
      "Heretics' Graves"
      mempty
      Square
      [Triangle, Circle]
      TheWagesOfSin

hereticsGravesSpectral_172 :: CardDef
hereticsGravesSpectral_172 =
  singleSided
    $ location
      "05172b"
      "Heretics' Graves"
      [Spectral]
      Square
      [Triangle, Circle]
      TheWagesOfSin

chapelCrypt_173 :: CardDef
chapelCrypt_173 =
  singleSided
    $ location
      "05173"
      "Chapel Crypt"
      mempty
      Diamond
      [Plus, Moon]
      TheWagesOfSin

chapelCryptSpectral_173 :: CardDef
chapelCryptSpectral_173 =
  singleSided
    $ location
      "05173b"
      "Chapel Crypt"
      [Spectral]
      Diamond
      [Plus, Moon]
      TheWagesOfSin

chapelCrypt_174 :: CardDef
chapelCrypt_174 =
  singleSided
    $ location
      "05174"
      "Chapel Crypt"
      mempty
      Diamond
      [Plus, Moon]
      TheWagesOfSin

chapelCryptSpectral_174 :: CardDef
chapelCryptSpectral_174 =
  singleSided
    $ location
      "05174b"
      "Chapel Crypt"
      [Spectral]
      Diamond
      [Plus, Moon]
      TheWagesOfSin

chapelAttic_175 :: CardDef
chapelAttic_175 =
  singleSided
    $ location
      "05175"
      "Chapel Attic"
      mempty
      Moon
      [Plus, Diamond]
      TheWagesOfSin

chapelAtticSpectral_175 :: CardDef
chapelAtticSpectral_175 =
  singleSided
    $ location
      "05175b"
      "Chapel Attic"
      [Spectral]
      Moon
      [Plus, Diamond]
      TheWagesOfSin

chapelAttic_176 :: CardDef
chapelAttic_176 =
  singleSided
    $ location
      "05176"
      "Chapel Attic"
      mempty
      Moon
      [Plus, Diamond]
      TheWagesOfSin

chapelAtticSpectral_176 :: CardDef
chapelAtticSpectral_176 =
  singleSided
    $ location
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
  victory 1
    $ location
      "05211"
      "Vault"
      [Lodge]
      Plus
      [Moon]
      ForTheGreaterGood

library :: CardDef
library =
  victory 1
    $ location
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
  victory 2
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ location
      "05257"
      "The Geist-Trap"
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

hangmansHillWhereItAllEnds :: CardDef
hangmansHillWhereItAllEnds =
  location
    "05302"
    ("Hangman's Hill" <:> "Where It All Ends")
    [Arkham]
    Moon
    [Plus]
    MusicOfTheDamned

silverTwilightLodgeShroudedInMystery :: CardDef
silverTwilightLodgeShroudedInMystery =
  victory 1
    $ location
      "05303"
      ("Silver Twilight Lodge" <:> "Shrouded In Mystery")
      [Arkham]
      Star
      [T]
      MusicOfTheDamned

hangmansHillShroudedInMystery :: CardDef
hangmansHillShroudedInMystery =
  victory 1
    $ location
      "05304"
      ("Hangman's Hill" <:> "Shrouded In Mystery")
      [Arkham]
      Moon
      [Plus]
      SecretsOfTheUniverse

silverTwilightLodgeWhereItAllEnds :: CardDef
silverTwilightLodgeWhereItAllEnds =
  location
    "05305"
    ("Silver Twilight Lodge" <:> "Where It All Ends")
    [Arkham]
    Star
    [T]
    SecretsOfTheUniverse

cosmicIngress :: CardDef
cosmicIngress =
  location
    "05332"
    "Cosmic Ingress"
    [Otherworld]
    NoSymbol
    []
    BeforeTheBlackThrone

hideousPalace :: CardDef
hideousPalace =
  locationWithUnrevealed
    "05333"
    "Cosmos"
    [Otherworld]
    NoSymbol
    []
    "Hideous Palace"
    [Otherworld]
    NoSymbol
    []
    BeforeTheBlackThrone

courtOfTheGreatOldOnes :: CardDef
courtOfTheGreatOldOnes =
  locationWithUnrevealed
    "05334"
    "Cosmos"
    [Otherworld]
    NoSymbol
    []
    "Court of the Great Old Ones"
    [Otherworld]
    NoSymbol
    []
    BeforeTheBlackThrone

theBlackThrone :: CardDef
theBlackThrone =
  locationWithUnrevealed
    "05335"
    "Cosmos"
    [Otherworld]
    NoSymbol
    []
    "The Black Throne"
    [Otherworld]
    NoSymbol
    []
    BeforeTheBlackThrone

dancersMist :: CardDef
dancersMist =
  quantity 3
    $ locationWithUnrevealed
      "05336"
      "Cosmos"
      [Otherworld]
      NoSymbol
      []
      "Dancer's Mist"
      [Otherworld, Void]
      NoSymbol
      []
      BeforeTheBlackThrone

flightIntoOblivion :: CardDef
flightIntoOblivion =
  quantity 3
    $ locationWithUnrevealed
      "05337"
      "Cosmos"
      [Otherworld]
      NoSymbol
      []
      "Flight into Oblivion"
      [Otherworld, Void]
      NoSymbol
      []
      BeforeTheBlackThrone

infinityOfDarkness :: CardDef
infinityOfDarkness =
  quantity 3
    $ locationWithUnrevealed
      "05338"
      "Cosmos"
      [Otherworld]
      NoSymbol
      []
      "Infinity of Darkness"
      [Otherworld, Void]
      NoSymbol
      []
      BeforeTheBlackThrone

cosmicGate :: CardDef
cosmicGate =
  locationWithUnrevealed
    "05339"
    "Cosmos"
    [Otherworld]
    NoSymbol
    []
    "Cosmic Gate"
    [Otherworld, Void]
    NoSymbol
    []
    BeforeTheBlackThrone

pathwayIntoVoid :: CardDef
pathwayIntoVoid =
  quantity 2
    $ locationWithUnrevealed
      "05340"
      "Cosmos"
      [Otherworld]
      NoSymbol
      []
      "Pathway into Void"
      [Otherworld, Void]
      NoSymbol
      []
      BeforeTheBlackThrone

dreamGateWondrousJourney :: CardDef
dreamGateWondrousJourney =
  (emptyCardDef "06015a" ("Dream-Gate" <:> "Wondrous Journey") LocationType)
    { cdRevealedName = Just $ "Dream-Gate" <:> "Wondrous Journey"
    , cdCardTraits = setFromList [Dreamlands]
    , cdRevealedCardTraits = setFromList [Dreamlands]
    , cdArt = "06015a"
    , cdLocationSymbol = Just NoSymbol
    , cdLocationRevealedSymbol = Just NoSymbol
    , cdLocationConnections = mempty
    , cdLocationRevealedConnections = mempty
    , cdClassSymbols = singleton #neutral
    , cdLevel = Nothing
    }

dreamGatePointlessReality :: CardDef
dreamGatePointlessReality =
  (emptyCardDef "06015b" ("Dream-Gate" <:> "Pointless Reality") LocationType)
    { cdRevealedName = Just $ "Dream-Gate" <:> "Pointless Reality"
    , cdCardTraits = setFromList [Dreamlands]
    , cdRevealedCardTraits = setFromList [Dreamlands]
    , cdDoubleSided = False
    , cdArt = "06015b"
    , cdLocationSymbol = Just NoSymbol
    , cdLocationRevealedSymbol = Just NoSymbol
    , cdLocationConnections = mempty
    , cdLocationRevealedConnections = mempty
    , cdClassSymbols = singleton #neutral
    , cdLevel = Nothing
    }

seventySteps :: CardDef
seventySteps =
  location
    "06045"
    ("Seventy Steps" <:> "Of Lighter Slumber")
    [Steps]
    Heart
    [Hourglass]
    BeyondTheGatesOfSleep

theCavernOfFlame :: CardDef
theCavernOfFlame =
  location
    "06046"
    "The Cavern of Flame"
    [Cave, Steps]
    Hourglass
    [Heart, Equals]
    BeyondTheGatesOfSleep

sevenHundredSteps :: CardDef
sevenHundredSteps =
  location
    "06047"
    ("Seven Hundred Steps" <:> "Of Deeper Slumber")
    [Steps]
    Equals
    [Hourglass, T]
    BeyondTheGatesOfSleep

baseOfTheSteps :: CardDef
baseOfTheSteps =
  location
    "06048"
    "Base of the Steps"
    [Steps, Woods]
    T
    [Equals, Squiggle]
    BeyondTheGatesOfSleep

theEnchantedPath :: CardDef
theEnchantedPath =
  location
    "06049"
    "The Enchanted Path"
    [Woods]
    Squiggle
    [T, Moon]
    BeyondTheGatesOfSleep

enchantedWoodsMysticalForest :: CardDef
enchantedWoodsMysticalForest =
  victory 2
    $ locationWithUnrevealed
      "06050"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Mystical Forest")
      [Woods]
      Circle
      [Squiggle, Star, Plus]
      BeyondTheGatesOfSleep

enchantedWoodsVillageOfZoogs :: CardDef
enchantedWoodsVillageOfZoogs =
  victory 2
    $ locationWithUnrevealed
      "06051"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Village of Zoogs")
      [Woods]
      Circle
      [Squiggle, Star, Plus]
      BeyondTheGatesOfSleep

enchantedWoodsGreatStoneCircle :: CardDef
enchantedWoodsGreatStoneCircle =
  victory 1
    $ locationWithUnrevealed
      "06052"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Great Stone Circle")
      [Woods]
      Triangle
      [Squiggle, Plus, Square]
      BeyondTheGatesOfSleep

enchantedWoodsStoneTrapdoor :: CardDef
enchantedWoodsStoneTrapdoor =
  victory 2
    $ locationWithUnrevealed
      "06053"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Stone Trapdoor")
      [Woods]
      Square
      [Squiggle, Triangle, Diamond]
      BeyondTheGatesOfSleep

enchantedWoodsTheMoonTree :: CardDef
enchantedWoodsTheMoonTree =
  victory 2
    $ locationWithUnrevealed
      "06054"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "The Moon-Tree")
      [Woods]
      Star
      [Squiggle, Diamond, Circle]
      BeyondTheGatesOfSleep

enchantedWoodsFungalForest :: CardDef
enchantedWoodsFungalForest =
  victory 2
    $ locationWithUnrevealed
      "06055"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Fungal Forest")
      [Woods]
      Plus
      [Squiggle, Circle, Triangle]
      BeyondTheGatesOfSleep

enchantedWoodsLostWoods :: CardDef
enchantedWoodsLostWoods =
  victory 2
    $ locationWithUnrevealed
      "06056"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Lost Woods")
      [Woods]
      Droplet
      [Squiggle]
      BeyondTheGatesOfSleep

waitingRoom :: CardDef
waitingRoom =
  location
    "06070"
    "Waiting Room"
    [StMarys]
    Circle
    [Diamond, Triangle, Square]
    WakingNightmare

emergencyRoom :: CardDef
emergencyRoom =
  victory 1
    $ location
      "06071"
      "Emergency Room"
      [StMarys]
      Square
      [Circle, Triangle]
      WakingNightmare

experimentalTherapiesWard :: CardDef
experimentalTherapiesWard =
  victory 2
    $ location
      "06072"
      "Experimental Therapies Ward"
      [StMarys]
      Triangle
      [Circle, Square, Heart]
      WakingNightmare

recordsOffice :: CardDef
recordsOffice =
  victory 2
    $ location
      "06073"
      "Records Office"
      [StMarys]
      Diamond
      [Circle]
      WakingNightmare

stairwell :: CardDef
stairwell =
  victory 1
    $ location
      "06074"
      "Stairwell"
      [StMarys]
      Heart
      [Triangle, Plus, Hourglass, T, Moon]
      WakingNightmare

morgue :: CardDef
morgue =
  victory 2
    $ locationWithUnrevealed
      "06075"
      "Basement Door"
      [StMarys, Basement]
      Plus
      [Heart]
      "Morgue"
      [StMarys, Basement]
      Hourglass
      [Heart]
      WakingNightmare

operatingRoom :: CardDef
operatingRoom =
  victory 2
    $ locationWithUnrevealed
      "06076"
      "Basement Door"
      [StMarys, Basement]
      Plus
      [Heart]
      "Operating Room"
      [StMarys, Basement]
      T
      [Heart]
      WakingNightmare

privateRoom :: CardDef
privateRoom =
  locationWithUnrevealed
    "06077"
    "Basement Door"
    [StMarys, Basement]
    Plus
    [Heart]
    "Private Room"
    [StMarys, Basement]
    Moon
    [Heart]
    WakingNightmare

ulthar :: CardDef
ulthar = veiled $ location "06127" "Ulthar" [Skai, City] Heart [Squiggle] TheSearchForKadath

skaiRiver :: CardDef
skaiRiver =
  singleSided
    $ location "06128" "Skai River" [Skai, City] Squiggle [Heart, Triangle] TheSearchForKadath

dylathLeen :: CardDef
dylathLeen = veiled $ location "06129" "Dylath-Leen" [Skai, City, Port] Triangle [Squiggle] TheSearchForKadath

kadatheron :: CardDef
kadatheron =
  veiled
    $ location
      "06130"
      "Kadatheron"
      [Mnar, Ancient, City, Port]
      Circle
      [Square, Diamond]
      TheSearchForKadath

sarnath :: CardDef
sarnath =
  victory 1
    $ veiled
    $ location "06131" "Sarnath" [Mnar, Ancient, Ruins] Diamond [Circle, Square] TheSearchForKadath

ruinsOfIb :: CardDef
ruinsOfIb =
  victory 1
    $ veiled
    $ location "06132" "Ruins of Ib" [Mnar, Ancient, Ruins] Square [Circle, Diamond] TheSearchForKadath

ilekVad :: CardDef
ilekVad =
  victory 1
    $ veiled
    $ location "06133" "Ilek-Vad" [Forbidden, City, Port] Circle [Diamond] TheSearchForKadath

forbiddenLands :: CardDef
forbiddenLands =
  singleSided
    $ location "06134" "Forbidden Lands" [Forbidden, Wastes] Diamond [Circle, Square] TheSearchForKadath

zulanThek :: CardDef
zulanThek =
  veiled
    $ location "06135" "Zulan-Thek" [Forbidden, City] Square [Diamond] TheSearchForKadath

baharna :: CardDef
baharna =
  victory 1
    $ veiled
    $ location "06136" "Baharna" [Oriab, City, Port] Circle [Square, Diamond] TheSearchForKadath

mtNgranek :: CardDef
mtNgranek =
  victory 1
    $ veiled
    $ location "06137" "Mt. Ngranek" [Oriab, Mountain] Square [Circle, Diamond] TheSearchForKadath

namelessRuins :: CardDef
namelessRuins =
  victory 1
    $ veiled
    $ location
      "06138"
      "Nameless Ruins"
      [Oriab, Ancient, Ruins]
      Diamond
      [Circle, Square]
      TheSearchForKadath

celephais :: CardDef
celephais =
  victory 1
    $ veiled
    $ location
      "06139"
      "Celephaïs"
      [OothNargai, City, Port]
      Hourglass
      [Moon, Plus]
      TheSearchForKadath

serannian :: CardDef
serannian =
  victory 1
    $ veiled
    $ location
      "06140"
      "Serannian"
      [OothNargai, City, Port]
      Moon
      [Hourglass]
      TheSearchForKadath

hazuthKleg :: CardDef
hazuthKleg =
  veiled
    $ location
      "06141"
      "Hazuth-Kleg"
      [OothNargai, City]
      Plus
      [Hourglass, T]
      TheSearchForKadath

templeOfUnattainableDesires :: CardDef
templeOfUnattainableDesires =
  victory 1
    $ veiled
    $ location
      "06142"
      "Temple of Unattainable Desires"
      [OothNargai, Temple]
      T
      [Plus, Star]
      TheSearchForKadath

cityWhichAppearsOnNoMap :: CardDef
cityWhichAppearsOnNoMap =
  victory 2
    $ veiled
    $ location
      "06143"
      "City-Which-Appears-On-No-Map"
      [City, Otherworld]
      Star
      [T]
      TheSearchForKadath

burialGround :: CardDef
burialGround =
  victory 1
    $ location
      "06174"
      "Burial Ground"
      [Graveyard]
      Moon
      [Square, Plus]
      AThousandShapesOfHorror

frontPorchEntryway :: CardDef
frontPorchEntryway =
  locationWithUnrevealed
    "06175"
    "Front Porch"
    mempty
    Square
    [Moon, Heart, Hourglass]
    "Entryway"
    mempty
    Square
    [Moon, Heart, Hourglass]
    AThousandShapesOfHorror

downstairsDoorwayDen :: CardDef
downstairsDoorwayDen =
  victory 1
    $ locationWithUnrevealed
      "06176"
      "Downstairs Doorway"
      mempty
      Hourglass
      [Square]
      "Den"
      mempty
      Hourglass
      [Square]
      AThousandShapesOfHorror

downstairsDoorwayParlor :: CardDef
downstairsDoorwayParlor =
  victory 1
    $ locationWithUnrevealed
      "06177"
      "Downstairs Doorway"
      mempty
      Hourglass
      [Square]
      "Parlor"
      mempty
      Hourglass
      [Square]
      AThousandShapesOfHorror

upstairsHallway :: CardDef
upstairsHallway =
  location
    "06178"
    "Upstairs Hallway"
    mempty
    Heart
    [Diamond, Square, Circle]
    AThousandShapesOfHorror

upstairsDoorwayLibrary :: CardDef
upstairsDoorwayLibrary =
  victory 1
    $ locationWithUnrevealed
      "06179"
      "Upstairs Doorway"
      mempty
      Diamond
      [Heart]
      "Library"
      mempty
      Diamond
      [Heart]
      AThousandShapesOfHorror

upstairsDoorwayBedroom :: CardDef
upstairsDoorwayBedroom =
  victory 1
    $ locationWithUnrevealed
      "06180"
      "Upstairs Doorway"
      mempty
      Diamond
      [Heart]
      "Bedroom"
      mempty
      Diamond
      [Heart]
      AThousandShapesOfHorror

attic_AThousandShapesOfHorror :: CardDef
attic_AThousandShapesOfHorror =
  victory 1
    $ location
      "06181"
      "Attic"
      mempty
      Circle
      [Heart]
      AThousandShapesOfHorror

unmarkedTomb :: CardDef
unmarkedTomb =
  victory 1
    $ location
      "06182"
      "Unmarked Tomb"
      [Graveyard]
      Plus
      [Moon]
      AThousandShapesOfHorror

mysteriousStairs_183 :: CardDef
mysteriousStairs_183 =
  location
    "06183"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

mysteriousStairs_184 :: CardDef
mysteriousStairs_184 =
  location
    "06184"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

mysteriousStairs_185 :: CardDef
mysteriousStairs_185 =
  location
    "06185"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

mysteriousStairs_186 :: CardDef
mysteriousStairs_186 =
  location
    "06186"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

mysteriousStairs_187 :: CardDef
mysteriousStairs_187 =
  location
    "06187"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

mysteriousStairs_188 :: CardDef
mysteriousStairs_188 =
  location
    "06188"
    "Mysterious Stairs"
    [Graveyard]
    NoSymbol
    mempty
    AThousandShapesOfHorror

moonBeastGalley :: CardDef
moonBeastGalley =
  storyOnBack
    $ location
      "06214"
      "Moon-Beast Galley"
      [Ship]
      NoSymbol
      []
      DarkSideOfTheMoon

cityOfTheMoonBeasts :: CardDef
cityOfTheMoonBeasts =
  victory 1
    $ location
      "06215"
      "City of the Moon-Beasts"
      [Surface, City]
      Triangle
      [Square, Moon]
      DarkSideOfTheMoon

theDarkCrater :: CardDef
theDarkCrater =
  victory 1
    $ location
      "06216"
      "The Dark Crater"
      [Surface]
      Moon
      [Triangle, Circle, Squiggle]
      DarkSideOfTheMoon

templeOfTheMoonLizard :: CardDef
templeOfTheMoonLizard =
  victory 1
    $ location
      "06217"
      "Temple of the Moon Lizard"
      [Surface]
      Square
      [Circle, Triangle]
      DarkSideOfTheMoon

moonForest :: CardDef
moonForest =
  victory 1
    $ location
      "06218"
      "Moon-Forest"
      [Surface, Woods]
      Circle
      [Moon, Square, Squiggle]
      DarkSideOfTheMoon

cavernsBeneathTheMoonDarkSide :: CardDef
cavernsBeneathTheMoonDarkSide =
  location
    "06219"
    ("Caverns Beneath the Moon" <:> "Dark Side")
    [Cave]
    Squiggle
    [Circle, Moon, Star]
    DarkSideOfTheMoon

theBlackCore :: CardDef
theBlackCore =
  location
    "06220"
    "The Black Core"
    [Cave]
    Star
    [Squiggle, Equals]
    DarkSideOfTheMoon

cavernsBeneathTheMoonLightSide :: CardDef
cavernsBeneathTheMoonLightSide =
  location
    "06221"
    ("Caverns Beneath the Moon" <:> "Light Side")
    [Cave]
    Equals
    [Star, Hourglass]
    DarkSideOfTheMoon

lightSideOfTheMoon :: CardDef
lightSideOfTheMoon =
  victory 2
    $ location
      "06222"
      "Light Side of the Moon"
      [Surface, Ruins]
      Hourglass
      [Equals, Heart]
      DarkSideOfTheMoon

theWhiteShip :: CardDef
theWhiteShip =
  location
    "06223"
    "The White Ship"
    [Ship]
    Heart
    [Hourglass]
    DarkSideOfTheMoon

vaultsOfZin :: CardDef
vaultsOfZin =
  victory 1
    $ veiled
    $ location
      "06254"
      "Vaults of Zin"
      []
      Heart
      [T, Moon]
      PointOfNoReturn

cityOfGugs :: CardDef
cityOfGugs =
  victory 1
    $ singleSided
    $ location
      "06255"
      "City of Gugs"
      []
      T
      [Heart, Squiggle, Moon]
      PointOfNoReturn

towerOfKoth :: CardDef
towerOfKoth =
  singleSided
    $ location
      "06256"
      "Tower of Koth"
      []
      Squiggle
      [T, Square]
      PointOfNoReturn

plainOfTheGhouls :: CardDef
plainOfTheGhouls =
  victory 1
    $ veiled
    $ location
      "06257"
      "Plain of the Ghouls"
      [Central]
      Moon
      [Heart, T, Hourglass]
      PointOfNoReturn

cragOfTheGhouls :: CardDef
cragOfTheGhouls =
  victory 1
    $ veiled
    $ location
      "06258"
      "Crag of the Ghouls"
      [Vale]
      Hourglass
      [Equals, Circle, Moon]
      PointOfNoReturn

seaOfBones :: CardDef
seaOfBones =
  victory 1
    $ singleSided
    $ location
      "06259"
      "Sea of Bones"
      [Vale]
      Circle
      [Hourglass, Star, Equals]
      PointOfNoReturn

peaksOfThok :: CardDef
peaksOfThok =
  singleSided
    $ location
      "06260"
      "Peaks of Thok"
      [Vale, Central]
      Star
      [Equals, Circle]
      PointOfNoReturn

valeOfPnath :: CardDef
valeOfPnath =
  victory 1
    $ veiled
    $ location
      "06261"
      "Vale of Pnath"
      [Vale]
      Equals
      [Hourglass, Star, Circle, Plus]
      PointOfNoReturn

seaOfPitch_262 :: CardDef
seaOfPitch_262 =
  veiled
    $ location
      "06262"
      "Sea of Pitch"
      [Depths]
      Plus
      [Equals, Plus]
      PointOfNoReturn

seaOfPitch_263 :: CardDef
seaOfPitch_263 =
  veiled
    $ location
      "06263"
      "Sea of Pitch"
      [Depths]
      Plus
      [Equals, Plus]
      PointOfNoReturn

seaOfPitch_264 :: CardDef
seaOfPitch_264 =
  veiled
    $ location
      "06264"
      "Sea of Pitch"
      [Depths]
      Plus
      [Equals, Plus]
      PointOfNoReturn

seaOfPitch_265 :: CardDef
seaOfPitch_265 =
  veiled
    $ location
      "06265"
      "Sea of Pitch"
      [Depths]
      Plus
      [Equals, Plus]
      PointOfNoReturn

plateauOfLengWhereTheGodsDwell :: CardDef
plateauOfLengWhereTheGodsDwell =
  location
    "06295"
    "Plateau of Leng"
    [Leng]
    Diamond
    [Triangle]
    WhereTheGodsDwell

coldWastes :: CardDef
coldWastes =
  location
    "06296"
    "Cold Wastes"
    [Leng]
    Triangle
    [Diamond, Plus, T]
    WhereTheGodsDwell

monasteryOfLeng :: CardDef
monasteryOfLeng =
  location
    "06297"
    "Monastery of Leng"
    [Leng]
    Plus
    [Triangle]
    WhereTheGodsDwell

onyxGates :: CardDef
onyxGates =
  location
    "06298"
    "Onyx Gates"
    [Leng, Kadath]
    T
    [Triangle, Square]
    WhereTheGodsDwell

theOnyxCastle :: CardDef
theOnyxCastle =
  locationWithUnrevealed
    "06299"
    "The Onyx Castle"
    [Kadath]
    Square
    [T]
    "The Great Hall"
    [Kadath]
    Square
    [Equals]
    WhereTheGodsDwell

forsakenTowerOfIllusionAndMyth :: CardDef
forsakenTowerOfIllusionAndMyth =
  locationWithUnrevealed
    "06300"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of Illusion and Myth")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

forsakenTowerOfLifeAndDeath :: CardDef
forsakenTowerOfLifeAndDeath =
  locationWithUnrevealed
    "06301"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of Life and Death")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

forsakenTowerOfInfiniteTruth :: CardDef
forsakenTowerOfInfiniteTruth =
  locationWithUnrevealed
    "06302"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of Infinite Truth")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

forsakenTowerOfEternalFlame :: CardDef
forsakenTowerOfEternalFlame =
  locationWithUnrevealed
    "06303"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of Eternal Flame")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

forsakenTowerOfTheQueenOfNight :: CardDef
forsakenTowerOfTheQueenOfNight =
  locationWithUnrevealed
    "06304"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of the Queen of Night")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

forsakenTowerOfPrimevalLight :: CardDef
forsakenTowerOfPrimevalLight =
  locationWithUnrevealed
    "06305"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of Primeval Light")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

theGreatWebWebStairs :: CardDef
theGreatWebWebStairs =
  quantity 3
    $ locationWithUnrevealed
      "06340"
      "The Great Web"
      [Otherworld, Extradimensional]
      NoSymbol
      []
      ("The Great Web" <:> "Web-Stairs")
      [Otherworld, Extradimensional]
      NoSymbol
      []
      WeaverOfTheCosmos

theGreatWebCosmicWeb :: CardDef
theGreatWebCosmicWeb =
  quantity 2
    $ locationWithUnrevealed
      "06341"
      "The Great Web"
      [Otherworld, Extradimensional]
      NoSymbol
      []
      ("The Great Web" <:> "Cosmic Web")
      [Otherworld, Extradimensional]
      NoSymbol
      []
      WeaverOfTheCosmos

theGreatWebTangledWeb :: CardDef
theGreatWebTangledWeb =
  quantity 3
    $ locationWithUnrevealed
      "06342"
      "The Great Web"
      [Otherworld, Extradimensional]
      NoSymbol
      []
      ("The Great Web" <:> "Tangled Web")
      [Otherworld, Extradimensional]
      NoSymbol
      []
      WeaverOfTheCosmos

theGreatWebPrisonOfCocoons :: CardDef
theGreatWebPrisonOfCocoons =
  quantity 2
    $ locationWithUnrevealed
      "06343"
      "The Great Web"
      [Otherworld, Extradimensional]
      NoSymbol
      []
      ("The Great Web" <:> "Prison of Cocoons")
      [Otherworld, Extradimensional]
      NoSymbol
      []
      WeaverOfTheCosmos

theGreatWebVastWeb :: CardDef
theGreatWebVastWeb =
  quantity 2
    $ locationWithUnrevealed
      "06344"
      "The Great Web"
      [Otherworld, Extradimensional]
      NoSymbol
      []
      ("The Great Web" <:> "Vast Web")
      [Otherworld, Extradimensional]
      NoSymbol
      []
      WeaverOfTheCosmos

theGreatWebWebWovenIsland :: CardDef
theGreatWebWebWovenIsland =
  quantity 3
    $ locationWithUnrevealed
      "06345"
      "The Great Web"
      [Otherworld, Extradimensional]
      NoSymbol
      []
      ("The Great Web" <:> "Web-Woven Island")
      [Otherworld, Extradimensional]
      NoSymbol
      []
      WeaverOfTheCosmos

unfamiliarChamber :: CardDef
unfamiliarChamber =
  location
    "07047"
    "Unfamiliar Chamber"
    [Cave]
    NoSymbol
    []
    ThePitOfDespair

boneRiddenPit :: CardDef
boneRiddenPit =
  locationWithUnrevealed
    "07048"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Bone-Ridden Pit"
    [Cave]
    NoSymbol
    []
    ThePitOfDespair

fishGraveyard :: CardDef
fishGraveyard =
  locationWithUnrevealed
    "07049"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Fish Graveyard"
    [Cave]
    NoSymbol
    []
    ThePitOfDespair

altarToDagon :: CardDef
altarToDagon =
  locationWithUnrevealed
    "07050"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Altar to Dagon"
    [Cave]
    NoSymbol
    []
    ThePitOfDespair

idolChamber :: CardDef
idolChamber =
  locationWithUnrevealed
    "07051"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Idol Chamber"
    [Cave]
    NoSymbol
    []
    ThePitOfDespair

sealedExit :: CardDef
sealedExit =
  locationWithUnrevealed
    "07052"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Sealed Exit"
    [Cave]
    NoSymbol
    []
    ThePitOfDespair

marshRefinery :: CardDef
marshRefinery =
  location
    "07063"
    "Marsh Refinery"
    [Innsmouth]
    Circle
    [Triangle, Hourglass, Moon, Square, Heart]
    TheVanishingOfElinaHarper

gilmanHouse :: CardDef
gilmanHouse =
  location
    "07064"
    "Gilman House"
    [Innsmouth]
    Diamond
    [Triangle, Droplet, Star, Squiggle, T]
    TheVanishingOfElinaHarper

innsmouthSquare :: CardDef
innsmouthSquare =
  location
    "07065"
    "Innsmouth Square"
    [Innsmouth, Central]
    Triangle
    [Circle, Diamond, Star, Hourglass, Square, T]
    TheVanishingOfElinaHarper

innsmouthHarbour :: CardDef
innsmouthHarbour =
  location
    "07066"
    "Innsmouth Harbour"
    [Innsmouth]
    Moon
    [Circle, Hourglass, Equals, Heart]
    TheVanishingOfElinaHarper

firstNationalGrocery :: CardDef
firstNationalGrocery =
  location
    "07067"
    "First National Grocery"
    [Innsmouth]
    Star
    [Triangle, Diamond, Droplet, Square, Plus]
    TheVanishingOfElinaHarper

fishStreetBridge :: CardDef
fishStreetBridge =
  location
    "07068"
    "Fish Street Bridge"
    [Innsmouth]
    Hourglass
    [Triangle, Circle, Moon, Equals, T]
    TheVanishingOfElinaHarper

theLittleBookshop :: CardDef
theLittleBookshop =
  location
    "07069"
    "The Little Bookshop"
    [Innsmouth]
    Droplet
    [Diamond, Star, Squiggle, Plus]
    TheVanishingOfElinaHarper

esotericOrderOfDagon :: CardDef
esotericOrderOfDagon =
  singleSided
    $ victory 1
    $ location
      "07070"
      "Esoteric Order of Dagon"
      [Innsmouth, Hideout]
      Plus
      [Droplet, Star]
      TheVanishingOfElinaHarper

sawboneAlley :: CardDef
sawboneAlley =
  singleSided
    $ victory 1
    $ location
      "07071"
      "Sawbone Alley"
      [Innsmouth, Hideout]
      Squiggle
      [Droplet, Diamond]
      TheVanishingOfElinaHarper

shorewardSlums :: CardDef
shorewardSlums =
  singleSided
    $ victory 1
    $ location
      "07072"
      "Shoreward Slums"
      [Innsmouth, Hideout]
      Equals
      [Hourglass, Moon]
      TheVanishingOfElinaHarper

theHouseOnWaterStreet :: CardDef
theHouseOnWaterStreet =
  singleSided
    $ victory 1
    $ location
      "07073"
      "The House on Water Street"
      [Innsmouth, Hideout]
      Heart
      [Circle, Moon]
      TheVanishingOfElinaHarper

innsmouthJail :: CardDef
innsmouthJail =
  singleSided
    $ victory 1
    $ location
      "07074"
      "Innsmouth Jail"
      [Innsmouth, Hideout]
      T
      [Diamond, Triangle, Hourglass]
      TheVanishingOfElinaHarper

newChurchGreen :: CardDef
newChurchGreen =
  singleSided
    $ victory 1
    $ location
      "07075"
      "New Church Green"
      [Innsmouth, Hideout]
      Square
      [Circle, Triangle, Star]
      TheVanishingOfElinaHarper

underwaterCavern :: CardDef
underwaterCavern =
  quantity 2
    $ locationWithUnrevealed
      "07102"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      "Underwater Cavern"
      [Cave]
      NoSymbol
      []
      FloodedCaverns

tidalPool :: CardDef
tidalPool =
  quantity 2
    $ locationWithUnrevealed
      "07103"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      "Tidal Pool"
      [Cave]
      NoSymbol
      []
      FloodedCaverns

undergroundRiver :: CardDef
undergroundRiver =
  victory 1
    $ quantity 2
    $ locationWithUnrevealed
      "07104"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      "Underground River"
      [Cave]
      NoSymbol
      []
      FloodedCaverns

esotericOrderOfDagonInTooDeep :: CardDef
esotericOrderOfDagonInTooDeep =
  location
    "07129"
    "Esoteric Order of Dagon"
    [Innsmouth, Midtown]
    NoSymbol
    []
    InTooDeep

sawboneAlleyInTooDeep :: CardDef
sawboneAlleyInTooDeep =
  victory 1
    $ location
      "07130"
      "Sawbone Alley"
      [Innsmouth]
      NoSymbol
      []
      InTooDeep

shorewardSlumsInTooDeep :: CardDef
shorewardSlumsInTooDeep =
  location
    "07131"
    "Shoreward Slums"
    [Innsmouth, Coastal, Midtown]
    NoSymbol
    []
    InTooDeep

theHouseOnWaterStreetInTooDeep :: CardDef
theHouseOnWaterStreetInTooDeep =
  location
    "07132"
    "The House on Water Street"
    [Innsmouth, Coastal]
    NoSymbol
    []
    InTooDeep

innsmouthJailInTooDeep :: CardDef
innsmouthJailInTooDeep =
  victory 1
    $ location
      "07133"
      "Innsmouth Jail"
      [Innsmouth, Midtown]
      NoSymbol
      []
      InTooDeep

newChurchGreenInTooDeep :: CardDef
newChurchGreenInTooDeep =
  location
    "07134"
    "New Church Green"
    [Innsmouth, Midtown]
    NoSymbol
    []
    InTooDeep

theLittleBookshopInTooDeep :: CardDef
theLittleBookshopInTooDeep =
  location
    "07135"
    "The Little Bookshop"
    [Innsmouth]
    NoSymbol
    []
    InTooDeep

fishStreetBridgeInTooDeep :: CardDef
fishStreetBridgeInTooDeep =
  victory 1
    $ location "07136" "Fish Street Bridge" [Innsmouth, Coastal, Midtown] NoSymbol [] InTooDeep

firstNationalGroceryInTooDeep :: CardDef
firstNationalGroceryInTooDeep =
  victory 1 $ location "07137" "First National Grocery" [Innsmouth, Midtown] NoSymbol [] InTooDeep

innsmouthHarbourInTooDeep :: CardDef
innsmouthHarbourInTooDeep =
  location "07138" "Fish Street Bridge" [Innsmouth, Coastal, Midtown] NoSymbol [] InTooDeep

innsmouthSquareInTooDeep :: CardDef
innsmouthSquareInTooDeep =
  victory 1 $ location "07139" "Innsmouth Square" [Innsmouth, Midtown] NoSymbol [] InTooDeep

gilmanHouseInTooDeep :: CardDef
gilmanHouseInTooDeep = location "07140" "Gilman House" [Innsmouth, Midtown] NoSymbol [] InTooDeep

marshRefineryInTooDeep :: CardDef
marshRefineryInTooDeep = location "07141" "Marsh Refinery" [Innsmouth, Coastal, Midtown] NoSymbol [] InTooDeep

railroadStation :: CardDef
railroadStation = location "07142" "Railroad Station" [Innsmouth] NoSymbol [] InTooDeep

desolateCoastline :: CardDef
desolateCoastline = location "07143" "Desolate Coastline" [Innsmouth, Coastal] NoSymbol [] InTooDeep

churningWaters :: CardDef
churningWaters =
  location
    "07168"
    "Churning Waters"
    [Ocean]
    Triangle
    [Circle, Square, Heart, Star, Diamond, Plus]
    DevilReef

lonelyIsle :: CardDef
lonelyIsle =
  locationWithUnrevealed
    "07169"
    "Devil Reef"
    [Ocean, Island]
    Circle
    [Triangle]
    "Lonely Isle"
    [Ocean, Island]
    Square
    [Triangle, Plus, Diamond]
    DevilReef

hiddenCove :: CardDef
hiddenCove =
  locationWithUnrevealed
    "07170"
    "Devil Reef"
    [Ocean, Island]
    Circle
    [Triangle]
    "Hidden Cove"
    [Ocean, Island]
    Heart
    [Triangle, Diamond, Star]
    DevilReef

wavewornIsland :: CardDef
wavewornIsland =
  locationWithUnrevealed
    "07171"
    "Devil Reef"
    [Ocean, Island]
    Circle
    [Triangle]
    "Waveworn Island"
    [Ocean, Island]
    Star
    [Triangle, Plus, Heart]
    DevilReef

saltMarshes :: CardDef
saltMarshes =
  locationWithUnrevealed
    "07172"
    "Devil Reef"
    [Ocean, Island]
    Circle
    [Triangle]
    "Salt Marshes"
    [Ocean, Island]
    Diamond
    [Triangle, Heart, Square]
    DevilReef

blackReef :: CardDef
blackReef =
  locationWithUnrevealed
    "07173"
    "Devil Reef"
    [Ocean, Island]
    Circle
    [Triangle]
    "Black Reef"
    [Ocean, Island]
    Plus
    [Triangle, Square, Star]
    DevilReef

bootleggersHideaway_174a :: CardDef
bootleggersHideaway_174a =
  victory 1
    $ locationWithUnrevealed
      "07174a"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      "Bootlegger's Hideaway"
      [Cave]
      NoSymbol
      []
      DevilReef

bootleggersHideaway_174b :: CardDef
bootleggersHideaway_174b =
  victory 1
    $ locationWithUnrevealed
      "07174b"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      "Bootlegger's Hideaway"
      [Cave]
      NoSymbol
      []
      DevilReef

deepOneGrotto_175a :: CardDef
deepOneGrotto_175a =
  locationWithUnrevealed
    "07175a"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    NoSymbol
    []
    "Deep One Grotto"
    [Cave, Yhanthlei]
    NoSymbol
    []
    DevilReef

deepOneGrotto_175b :: CardDef
deepOneGrotto_175b =
  locationWithUnrevealed
    "07175b"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    NoSymbol
    []
    "Deep One Grotto"
    [Cave, Yhanthlei]
    NoSymbol
    []
    DevilReef

cyclopeanRuins_176a :: CardDef
cyclopeanRuins_176a =
  locationWithUnrevealed
    "07176a"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    NoSymbol
    []
    "Cyclopean Ruins"
    [Cave, Yhanthlei]
    NoSymbol
    []
    DevilReef

cyclopeanRuins_176b :: CardDef
cyclopeanRuins_176b =
  locationWithUnrevealed
    "07176b"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    NoSymbol
    []
    "Cyclopean Ruins"
    [Cave, Yhanthlei]
    NoSymbol
    []
    DevilReef

templeOfTheUnion_177a :: CardDef
templeOfTheUnion_177a =
  locationWithUnrevealed
    "07177a"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    NoSymbol
    []
    "Temple of the Union"
    [Cave, Yhanthlei]
    NoSymbol
    []
    DevilReef

templeOfTheUnion_177b :: CardDef
templeOfTheUnion_177b =
  locationWithUnrevealed
    "07177b"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    NoSymbol
    []
    "Temple of the Union"
    [Cave, Yhanthlei]
    NoSymbol
    []
    DevilReef

oldInnsmouthRoad :: CardCode -> Name -> CardDef
oldInnsmouthRoad cardCode name =
  locationWithUnrevealed
    cardCode
    "Old Innsmouth Road"
    [Road]
    NoSymbol
    []
    name
    [Road]
    NoSymbol
    []
    HorrorInHighGear

falconPointApproach :: CardDef
falconPointApproach = oldInnsmouthRoad "07203" "Falcon Point Approach"

-- road 1
dimlyLitRoad_a :: CardDef
dimlyLitRoad_a = oldInnsmouthRoad "07204a" "Dimly Lit Road"

-- road 1
dimlyLitRoad_b :: CardDef
dimlyLitRoad_b = oldInnsmouthRoad "07204b" "Dimly Lit Road"

-- road 1
dimlyLitRoad_c :: CardDef
dimlyLitRoad_c = oldInnsmouthRoad "07204c" "Dimly Lit Road"

-- road 1
cliffsideRoad_a :: CardDef
cliffsideRoad_a = victory 1 $ oldInnsmouthRoad "07205a" "Cliffside Road"

-- road 1
cliffsideRoad_b :: CardDef
cliffsideRoad_b = victory 1 $ oldInnsmouthRoad "07205b" "Cliffside Road"

-- road 2
forkInTheRoad_a :: CardDef
forkInTheRoad_a = victory 1 $ oldInnsmouthRoad "07206a" "Fork in the Road"

-- road 2
forkInTheRoad_b :: CardDef
forkInTheRoad_b = victory 1 $ oldInnsmouthRoad "07206b" "Fork in the Road"

-- road 3
intersection_a :: CardDef
intersection_a = victory 1 $ oldInnsmouthRoad "07207a" "Intersection"

-- road 3
intersection_b :: CardDef
intersection_b = victory 1 $ oldInnsmouthRoad "07207b" "Intersection"

-- road 1
tightTurn_a :: CardDef
tightTurn_a = oldInnsmouthRoad "07208a" "Tight Turn"

-- road 1
tightTurn_b :: CardDef
tightTurn_b = oldInnsmouthRoad "07208b" "Tight Turn"

-- road 1
tightTurn_c :: CardDef
tightTurn_c = oldInnsmouthRoad "07208c" "Tight Turn"

-- road 1
desolateRoad_a :: CardDef
desolateRoad_a = victory 1 $ oldInnsmouthRoad "07209a" "Desolate Road"

-- road 1
desolateRoad_b :: CardDef
desolateRoad_b = victory 1 $ oldInnsmouthRoad "07209b" "Desolate Road"

-- road 1
longWayAround :: CardDef
longWayAround = quantity 6 $ oldInnsmouthRoad "07210" "Long Way Around"

falconPointGatehouse :: CardDef
falconPointGatehouse =
  location
    "07239"
    "Falcon Point Gatehouse"
    [FalconPoint]
    NoSymbol
    []
    ALightInTheFog

lighthouseStairwell :: CardDef
lighthouseStairwell =
  location
    "07240"
    "Lighthouse Stairwell"
    [FalconPoint]
    Equals
    [Triangle, Squiggle]
    ALightInTheFog

lanternRoom :: CardDef
lanternRoom =
  victory 1
    $ location
      "07241"
      "Lantern Room"
      [FalconPoint]
      Triangle
      [Equals]
      ALightInTheFog

falconPointCliffside :: CardDef
falconPointCliffside =
  location
    "07242"
    "Falcon Point Cliffside"
    [FalconPoint]
    NoSymbol
    []
    ALightInTheFog

lighthouseKeepersCottage :: CardDef
lighthouseKeepersCottage =
  location
    "07243"
    "Lighthouse Keeper's Cottage"
    [FalconPoint]
    NoSymbol
    []
    ALightInTheFog

sunkenGrottoUpperDepths :: CardDef
sunkenGrottoUpperDepths =
  locationWithUnrevealed
    "07244"
    "Lighthouse Basement"
    [Cave]
    Squiggle
    [Equals, Square, T]
    ("Sunken Grotto" <:> "Upper Depths")
    [Cave]
    Squiggle
    [Equals, Square, T]
    ALightInTheFog

sunkenGrottoLowerDepths :: CardDef
sunkenGrottoLowerDepths =
  location
    "07245"
    ("Sunken Grotto" <:> "Lower Depths")
    [Cave]
    Square
    [Squiggle, Diamond, T]
    ALightInTheFog

sunkenGrottoFinalDepths :: CardDef
sunkenGrottoFinalDepths =
  location
    "07246"
    ("Sunken Grotto" <:> "Final Depths")
    [Cave]
    Diamond
    [Square, T]
    ALightInTheFog

shrineToHydra :: CardDef
shrineToHydra =
  victory 1
    $ locationWithUnrevealed
      "07247"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      "Shrine to Hydra"
      [Cave]
      NoSymbol
      []
      ALightInTheFog

deepOneNursery :: CardDef
deepOneNursery =
  locationWithUnrevealed
    "07248"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Deep One Nursery"
    [Cave]
    NoSymbol
    []
    ALightInTheFog

theMoonRoom :: CardDef
theMoonRoom =
  locationWithUnrevealed
    "07249"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "The Moon Room"
    [Cave]
    NoSymbol
    []
    ALightInTheFog

sunkenArchives :: CardDef
sunkenArchives =
  locationWithUnrevealed
    "07250"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Sunken Archives"
    [Cave]
    NoSymbol
    []
    ALightInTheFog

pumpRoom :: CardDef
pumpRoom =
  locationWithUnrevealed
    "07251"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Pump Room"
    [Cave]
    NoSymbol
    []
    ALightInTheFog

holdingCells :: CardDef
holdingCells =
  victory 1
    $ singleSided
    $ location
      "07252b"
      "Holding Cells"
      [Cave]
      T
      [Squiggle, Square, Diamond]
      ALightInTheFog

grandEntryway :: CardDef
grandEntryway =
  location
    "07283"
    "Grand Entryway"
    [GroundFloor]
    Triangle
    [Square, Star, Plus, Trefoil]
    TheLairOfDagon

hallOfBlood :: CardDef
hallOfBlood =
  locationWithUnrevealed
    "07284"
    "First Floor Hall"
    [GroundFloor]
    Trefoil
    [Triangle]
    "Hall of Blood"
    [GroundFloor]
    Square
    [Triangle]
    TheLairOfDagon

hallOfTheDeep :: CardDef
hallOfTheDeep =
  victory 1
    $ locationWithUnrevealed
      "07285"
      "First Floor Hall"
      [GroundFloor]
      Trefoil
      [Triangle]
      "Hall of the Deep"
      [GroundFloor]
      Plus
      [Triangle]
      TheLairOfDagon

foulCorridors :: CardDef
foulCorridors =
  locationWithUnrevealed
    "07286"
    "Foul Corridors"
    [SecondFloor]
    Star
    [Triangle, Heart, Moon, Equals, Hourglass]
    "Foul Corridors"
    [SecondFloor, Passageway]
    Star
    [Triangle, Heart, Moon, Equals, Hourglass]
    TheLairOfDagon

hallOfLoyalty :: CardDef
hallOfLoyalty =
  victory 1
    $ locationWithUnrevealed
      "07287"
      "Second Floor Hall"
      [SecondFloor]
      Hourglass
      [Star]
      "Hall of Loyalty"
      [SecondFloor]
      Equals
      [Star]
      TheLairOfDagon

hallOfRebirth :: CardDef
hallOfRebirth =
  locationWithUnrevealed
    "07288"
    "Second Floor Hall"
    [SecondFloor]
    Hourglass
    [Star]
    "Hall of Rebirth"
    [SecondFloor]
    Heart
    [Star]
    TheLairOfDagon

hallOfSilence :: CardDef
hallOfSilence =
  locationWithUnrevealed
    "07289"
    "Third Floor Hall"
    [ThirdFloor]
    Moon
    [Star]
    "Hall of Silence"
    [ThirdFloor]
    Moon
    [Star]
    TheLairOfDagon

doorwayToTheDepths :: CardDef
doorwayToTheDepths =
  locationWithUnrevealed
    "07290"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Doorway to the Depths"
    [Cave]
    Circle
    [Diamond]
    TheLairOfDagon

lairOfDagon :: CardDef
lairOfDagon =
  victory 1
    $ location
      "07291"
      "Lair of Dagon"
      [Yhanthlei, Lair]
      Diamond
      [Circle]
      TheLairOfDagon

darkAbyss :: CardDef
darkAbyss =
  quantity 2
    $ locationWithUnrevealed
      "07319"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      "Dark Abyss"
      [Cave]
      NoSymbol
      []
      IntoTheMaelstrom

gatewayToYhanthlei :: CardDef
gatewayToYhanthlei =
  location
    "07320"
    "Gateway to Y'ha-nthlei"
    [Yhanthlei, Otherworld]
    NoSymbol
    []
    IntoTheMaelstrom

sunkenHalls :: CardDef
sunkenHalls =
  quantity 2
    $ locationWithUnrevealed
      "07321"
      "Y'ha-nthlei"
      [Yhanthlei]
      NoSymbol
      []
      "Sunken Halls"
      [Yhanthlei]
      NoSymbol
      []
      IntoTheMaelstrom

vaultOfRiches :: CardDef
vaultOfRiches =
  quantity 2
    $ locationWithUnrevealed
      "07322"
      "Y'ha-nthlei"
      [Yhanthlei]
      NoSymbol
      []
      "Vault of Riches"
      [Yhanthlei]
      NoSymbol
      []
      IntoTheMaelstrom

underseaCorridors :: CardDef
underseaCorridors =
  quantity 3
    $ locationWithUnrevealed
      "07323"
      "Y'ha-nthlei"
      [Yhanthlei]
      NoSymbol
      []
      "Undersea Corridors"
      [Yhanthlei]
      NoSymbol
      []
      IntoTheMaelstrom

statuesInTheDeep :: CardDef
statuesInTheDeep =
  locationWithUnrevealed
    "07324"
    "Y'ha-nthlei Sanctum"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    "Statues in the Deep"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    IntoTheMaelstrom

submergedTemple :: CardDef
submergedTemple =
  locationWithUnrevealed
    "07325"
    "Y'ha-nthlei Sanctum"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    "Submerged Temple"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    IntoTheMaelstrom

syzygyChamber :: CardDef
syzygyChamber =
  locationWithUnrevealed
    "07326"
    "Y'ha-nthlei Sanctum"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    "Syzygy Chamber"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    IntoTheMaelstrom

onyxGuardians :: CardDef
onyxGuardians =
  locationWithUnrevealed
    "07327"
    "Y'ha-nthlei Sanctum"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    "Onyx Guardians"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    IntoTheMaelstrom

lairOfDagonIntoTheMaelstrom :: CardDef
lairOfDagonIntoTheMaelstrom =
  location
    "07328"
    ("Lair of Dagon" <:> "Sanctuary of Father Dagon")
    [Yhanthlei, Lair]
    NoSymbol
    []
    IntoTheMaelstrom

lairOfHydra :: CardDef
lairOfHydra =
  location
    "07329"
    ("Lair of Hydra" <:> "High Temple of Mother Hydra")
    [Yhanthlei, Lair]
    NoSymbol
    []
    IntoTheMaelstrom

crashSite :: CardDef
crashSite =
  withMeta ("shelter", Number 0)
    $ location "08502" "Crash Site" mempty Circle [Diamond, Triangle, Heart] IceAndDeath

frozenShores :: CardDef
frozenShores =
  withMeta ("shelter", Number 2)
    $ location "08503" "Frozen Shores" [Mainland] Diamond [Circle, Triangle, Heart, Square] IceAndDeath

treacherousPath :: CardDef
treacherousPath =
  withMeta ("shelter", Number 1)
    $ location
      "08504"
      "Treacherous Path"
      [Mountains]
      Triangle
      [Circle, Diamond, Heart, Equals]
      IceAndDeath

precariousIceSheet :: CardDef
precariousIceSheet =
  withMeta ("shelter", Number 2)
    $ location
      "08505"
      "Precarious Ice Sheet"
      [Glacier]
      Heart
      [Circle, Diamond, Triangle, Droplet]
      IceAndDeath

broadSnowdrifts :: CardDef
broadSnowdrifts =
  withMeta ("shelter", Number 4)
    $ location
      "08506"
      "Broad Snowdrifts"
      [Mainland, Uncharted]
      Square
      [Diamond, Hourglass, Star, Squiggle]
      IceAndDeath

icyWastes :: CardDef
icyWastes =
  withMeta ("shelter", Number 4)
    $ location
      "08507"
      "Broad Snowdrifts"
      [Glacier, Uncharted]
      Droplet
      [Heart, Star, Moon, Trefoil]
      IceAndDeath

rockyCrags :: CardDef
rockyCrags =
  withMeta ("shelter", Number 3)
    $ location
      "08508"
      "Rocky Crags"
      [Mountains, Uncharted]
      Equals
      [Triangle, Hourglass, Moon, Plus]
      IceAndDeath

snowGraves :: CardDef
snowGraves =
  withMeta ("shelter", Number 5)
    $ location
      "08509"
      "Snow Graves"
      [Mainland, Uncharted]
      Squiggle
      [Square]
      IceAndDeath

icebreakerLanding :: CardDef
icebreakerLanding =
  withMeta ("shelter", Number 5)
    $ location
      "08510"
      "Icebreaker Landing"
      [Glacier, Uncharted]
      Trefoil
      [Droplet]
      IceAndDeath

frigidCave :: CardDef
frigidCave =
  withMeta ("shelter", Number 6)
    $ location
      "08511"
      "Frigid Cave"
      [Mountains, Uncharted]
      Plus
      [Equals]
      IceAndDeath

barrierCamp :: CardDef
barrierCamp =
  withMeta ("shelter", Number 7)
    $ location
      "08512"
      "Barrier Camp"
      [Glacier, Uncharted]
      Moon
      [Droplet, Equals]
      IceAndDeath

remnantsOfLakesCamp :: CardDef
remnantsOfLakesCamp =
  withMeta ("shelter", Number 7)
    $ location
      "08513"
      "Remnants of Lake's Camp"
      [Mainland, Uncharted]
      Star
      [Square, Droplet]
      IceAndDeath

crystallineCavern :: CardDef
crystallineCavern =
  withMeta ("shelter", Number 8)
    $ location
      "08514"
      "Crystalline Cavern"
      [Mountains, Uncharted]
      Hourglass
      [Equals, Square]
      IceAndDeath

prisonOfMemories :: CardDef
prisonOfMemories =
  singleSided
    $ location
      "08556"
      "Prison of Memories"
      [Otherworld, Mirage]
      Circle
      [Triangle]
      FatalMirage

baseCamp :: CardDef
baseCamp =
  singleSided
    $ location
      "08557"
      "Base Camp"
      [Mirage]
      Triangle
      [Circle, Triangle, Trefoil, Square, Spade]
      FatalMirage

deckOfTheTheodosia :: CardDef
deckOfTheTheodosia =
  singleSided
    $ location
      "08558"
      "Deck of the Theodosia"
      [Mirage]
      Triangle
      [Circle, Triangle, Hourglass, Square, Diamond]
      FatalMirage

universityHalls :: CardDef
universityHalls =
  singleSided
    $ location
      "08559"
      "University Halls"
      [Mirage]
      Triangle
      [Circle, Triangle, Moon, Trefoil, Hourglass]
      FatalMirage

hedgeMaze :: CardDef
hedgeMaze =
  singleSided
    $ location
      "08560"
      "Hedge Maze"
      [Mirage]
      Diamond
      [Circle, Equals]
      FatalMirage

desertedStation :: CardDef
desertedStation =
  singleSided
    $ location
      "08561"
      "Deserted Station"
      [Mirage]
      Spade
      [Circle, Heart]
      FatalMirage

coastalWaters :: CardDef
coastalWaters =
  singleSided
    $ location
      "08562"
      "Coastal Waters"
      [Mirage]
      Square
      [Circle, Star]
      FatalMirage

elderChamber :: CardDef
elderChamber =
  singleSided
    $ location
      "08563"
      "Elder Chamber"
      [Mirage]
      Moon
      [Circle, T]
      FatalMirage

riverviewTheatre :: CardDef
riverviewTheatre =
  singleSided
    $ location
      "08564"
      "Riverview Theatre"
      [Mirage]
      Trefoil
      [Circle, Squiggle]
      FatalMirage

standingStones :: CardDef
standingStones =
  singleSided
    $ location
      "08565"
      "Standing Stones"
      [Mirage]
      Hourglass
      [Circle, Droplet]
      FatalMirage

airfield :: CardDef
airfield =
  singleSided
    $ location
      "08566"
      "Airfield"
      [Mirage]
      Star
      [Circle]
      FatalMirage

alaskanWilds :: CardDef
alaskanWilds =
  singleSided
    $ location
      "08567"
      "Alaskan Wilds"
      [Mirage]
      Heart
      [Circle]
      FatalMirage

clutteredDormitory :: CardDef
clutteredDormitory =
  singleSided
    $ location
      "08568"
      "Cluttered Dormitory"
      [Mirage]
      T
      [Circle]
      FatalMirage

dyersClassroom :: CardDef
dyersClassroom =
  singleSided
    $ location
      "08569"
      "Dyer's Classroom"
      [Mirage]
      Droplet
      [Circle]
      FatalMirage

infirmaryFatalMirage :: CardDef
infirmaryFatalMirage =
  singleSided
    $ location
      "08570"
      "Infirmary"
      [Mirage]
      Squiggle
      [Circle]
      FatalMirage

drKenslersOffice :: CardDef
drKenslersOffice =
  singleSided
    $ location
      "08571"
      "Dr. Kensler's Office"
      [Mirage]
      Squiggle
      [Circle]
      FatalMirage

moaiStatues :: CardDef
moaiStatues =
  singleSided
    $ location
      "08572"
      "Mo'ai Statues"
      [Mirage]
      Equals
      [Circle]
      FatalMirage

ottomanFront :: CardDef
ottomanFront =
  singleSided
    $ location
      "08573"
      "Ottoman Front"
      [Mirage]
      Star
      [Circle]
      FatalMirage

theBlackStone :: CardDef
theBlackStone =
  singleSided
    $ location
      "08574"
      "The Black Stone"
      [Mirage]
      Droplet
      [Circle]
      FatalMirage

deepDrifts :: CardDef
deepDrifts =
  victory 1
    $ locationWithUnrevealed
      "08600"
      "Mountainside"
      mempty
      NoSymbol
      []
      "Deep Drifts"
      mempty
      NoSymbol
      []
      ToTheForbiddenPeaks

whiteBluff :: CardDef
whiteBluff =
  victory 1
    $ locationWithUnrevealed
      "08601"
      "Mountainside"
      mempty
      NoSymbol
      []
      "White Bluff"
      mempty
      NoSymbol
      []
      ToTheForbiddenPeaks

steepIncline :: CardDef
steepIncline =
  victory 1
    $ locationWithUnrevealed
      "08602"
      "Mountainside"
      mempty
      NoSymbol
      []
      "Steep Incline"
      mempty
      NoSymbol
      []
      ToTheForbiddenPeaks

narrowRidge :: CardDef
narrowRidge =
  victory 1
    $ locationWithUnrevealed
      "08603"
      "Mountainside"
      mempty
      NoSymbol
      []
      "Narrow Ridge"
      mempty
      NoSymbol
      []
      ToTheForbiddenPeaks

rockyLedge :: CardDef
rockyLedge =
  victory 1
    $ locationWithUnrevealed
      "08604"
      "Mountainside"
      mempty
      NoSymbol
      []
      "Rocky Ledge"
      mempty
      NoSymbol
      []
      ToTheForbiddenPeaks

snowCoveredCrag :: CardDef
snowCoveredCrag =
  victory 1
    $ locationWithUnrevealed
      "08605"
      "Mountainside"
      mempty
      NoSymbol
      []
      "Snow-Covered Crag"
      mempty
      NoSymbol
      []
      ToTheForbiddenPeaks

windsweptPath :: CardDef
windsweptPath =
  victory 1
    $ locationWithUnrevealed
      "08606"
      "Mountainside"
      mempty
      NoSymbol
      []
      "Windswept Path"
      mempty
      NoSymbol
      []
      ToTheForbiddenPeaks

theSummit :: CardDef
theSummit =
  victory 1
    $ location
      "08607"
      "The Summit"
      mempty
      NoSymbol
      []
      ToTheForbiddenPeaks

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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  victory 1
    $ locationWithUnrevealed
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
  (location "82006b" "Gondola" [Venice, Boat] NoSymbol [] CarnevaleOfHorrors)
    { cdDoubleSided = False
    }

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

room225 :: CardDef
room225 =
  location
    "84010"
    ("Room 225" <:> "Scene of the Crime")
    [CrimeScene]
    Circle
    [Square, Triangle]
    MurderAtTheExcelsiorHotel

suiteBalcony :: CardDef
suiteBalcony =
  location
    "84011"
    "Suite Balcony"
    [CrimeScene]
    Triangle
    [Circle]
    MurderAtTheExcelsiorHotel

secondFloorHall :: CardDef
secondFloorHall =
  location
    "84012"
    "Second Floor Hall"
    [Hall]
    Square
    [Circle, T, Squiggle, Plus, Diamond, Equals]
    MurderAtTheExcelsiorHotel

foyerMurderAtTheExcelsiorHotel :: CardDef
foyerMurderAtTheExcelsiorHotel =
  location
    "84013"
    "Foyer"
    [Hall]
    T
    [Square, Squiggle, Hourglass, Moon]
    MurderAtTheExcelsiorHotel

restaurant :: CardDef
restaurant =
  location
    "84014"
    "Restaurant"
    [Hall]
    Squiggle
    [Square, T]
    MurderAtTheExcelsiorHotel

hotelRoof :: CardDef
hotelRoof =
  victory 1
    $ location
      "84015"
      "Hotel Roof"
      []
      Plus
      [Square]
      MurderAtTheExcelsiorHotel

room212 :: CardDef
room212 =
  victory 1
    $ location
      "84016"
      "Room 212"
      [CrimeScene]
      Diamond
      [Square]
      MurderAtTheExcelsiorHotel

room245 :: CardDef
room245 =
  victory 1
    $ location
      "84017"
      "Room 245"
      [CrimeScene]
      Equals
      [Square]
      MurderAtTheExcelsiorHotel

officeMurderAtTheExcelsiorHotel :: CardDef
officeMurderAtTheExcelsiorHotel =
  victory 1
    $ location
      "84018"
      "Office"
      []
      Moon
      [T, Hourglass]
      MurderAtTheExcelsiorHotel

basement :: CardDef
basement =
  victory 1
    $ location
      "84019"
      "Basement"
      [CrimeScene]
      Hourglass
      [T, Moon]
      MurderAtTheExcelsiorHotel

betweenWorlds :: CardDef
betweenWorlds =
  location "xbetween" "Between Worlds" [Otherworld] NoSymbol [] ShatteredAeons

emptySpace :: CardDef
emptySpace =
  location "xempty" "EmptySpace" [] NoSymbol [] BeforeTheBlackThrone
