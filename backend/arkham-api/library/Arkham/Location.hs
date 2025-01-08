{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Location (
  module Arkham.Location,
  module X,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Location.Locations
import Arkham.Location.Runner
import Arkham.Location.Types as X (Location)

createLocation :: IsCard a => a -> LocationId -> Location
createLocation a lid = lookupLocation (toCardCode a) lid (toCardId a)

lookupLocation :: CardCode -> LocationId -> CardId -> Location
lookupLocation cCode = case lookup cCode allLocations of
  Nothing -> error $ "Unknown location: " <> show cCode
  Just (SomeLocationCard a) -> \lid cid -> Location $ cbCardBuilder a cid lid

instance RunMessage Location where
  runMessage (Reset target) x | isTarget (toAttrs x) target = do
    let a = toAttrs x
    pure $ lookupLocation (toCardCode a) a.id (toCardId a)
  runMessage msg x@(Location l) = do
    modifiers' <- getModifiers (toTarget x)
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    Location <$> runMessage msg' l

instance FromJSON Location where
  parseJSON = withObject "Location" $ \o -> do
    cCode <- o .: "cardCode"
    withLocationCardCode cCode
      $ \(_ :: LocationCard a) -> Location <$> parseJSON @a (Object o)

withLocationCardCode
  :: CardCode -> (forall a. IsLocation a => LocationCard a -> r) -> r
withLocationCardCode cCode f = case lookup cCode allLocations of
  Nothing -> error "invalid locations"
  Just (SomeLocationCard a) -> f a

allLocations :: Map CardCode SomeLocationCard
allLocations =
  mapFrom
    someLocationCardCode
    [ -- Night of the Zealot
      -- The Gathering
      SomeLocationCard study
    , SomeLocationCard hallway
    , SomeLocationCard attic
    , SomeLocationCard cellar
    , SomeLocationCard parlor
    , -- The Midnight Masks
      SomeLocationCard yourHouse
    , SomeLocationCard rivertown
    , SomeLocationCard southsideHistoricalSociety
    , SomeLocationCard southsideMasBoardingHouse
    , SomeLocationCard stMarysHospital
    , SomeLocationCard miskatonicUniversity
    , SomeLocationCard downtownFirstBankOfArkham
    , SomeLocationCard downtownArkhamAsylum
    , SomeLocationCard easttown
    , SomeLocationCard graveyard
    , SomeLocationCard northside
    , SomeLocationCard mainPath
    , -- The Devourer Below
      SomeLocationCard arkhamWoodsUnhallowedGround
    , SomeLocationCard arkhamWoodsTwistingPaths
    , SomeLocationCard arkhamWoodsOldHouse
    , SomeLocationCard arkhamWoodsCliffside
    , SomeLocationCard arkhamWoodsTangledThicket
    , SomeLocationCard arkhamWoodsQuietGlade
    , SomeLocationCard ritualSite
    , -- The Dunwich Legacy
      -- Extracurricular Activity
      SomeLocationCard miskatonicQuad
    , SomeLocationCard humanitiesBuilding
    , SomeLocationCard orneLibrary
    , SomeLocationCard studentUnion
    , SomeLocationCard dormitories
    , SomeLocationCard administrationBuilding
    , SomeLocationCard facultyOfficesTheNightIsStillYoung
    , SomeLocationCard facultyOfficesTheHourIsLate
    , SomeLocationCard scienceBuilding
    , SomeLocationCard alchemyLabs
    , -- The House Always Wins
      SomeLocationCard laBellaLuna
    , SomeLocationCard cloverClubLounge
    , SomeLocationCard cloverClubBar
    , SomeLocationCard cloverClubCardroom
    , SomeLocationCard darkenedHall
    , SomeLocationCard artGallery
    , SomeLocationCard vipArea
    , SomeLocationCard backAlley
    , -- The Miskatonic Museum
      SomeLocationCard museumEntrance
    , SomeLocationCard museumHalls
    , SomeLocationCard securityOffice_128
    , SomeLocationCard securityOffice_129
    , SomeLocationCard administrationOffice_130
    , SomeLocationCard administrationOffice_131
    , SomeLocationCard exhibitHallAthabaskanExhibit
    , SomeLocationCard exhibitHallMedusaExhibit
    , SomeLocationCard exhibitHallNatureExhibit
    , SomeLocationCard exhibitHallEgyptianExhibit
    , SomeLocationCard exhibitHallHallOfTheDead
    , SomeLocationCard exhibitHallRestrictedHall
    , -- The Essex County Express
      SomeLocationCard passengerCar_167
    , SomeLocationCard passengerCar_168
    , SomeLocationCard passengerCar_169
    , SomeLocationCard passengerCar_170
    , SomeLocationCard passengerCar_171
    , SomeLocationCard sleepingCar
    , SomeLocationCard diningCar
    , SomeLocationCard parlorCar
    , SomeLocationCard engineCar_175
    , SomeLocationCard engineCar_176
    , SomeLocationCard engineCar_177
    , -- Blood on the Altar
      SomeLocationCard villageCommons
    , SomeLocationCard bishopsBrook_202
    , SomeLocationCard bishopsBrook_203
    , SomeLocationCard burnedRuins_204
    , SomeLocationCard burnedRuins_205
    , SomeLocationCard osbornsGeneralStore_206
    , SomeLocationCard osbornsGeneralStore_207
    , SomeLocationCard congregationalChurch_208
    , SomeLocationCard congregationalChurch_209
    , SomeLocationCard houseInTheReeds_210
    , SomeLocationCard houseInTheReeds_211
    , SomeLocationCard schoolhouse_212
    , SomeLocationCard schoolhouse_213
    , SomeLocationCard theHiddenChamber
    , -- Undimensioned and Unseen
      SomeLocationCard dunwichVillage_242
    , SomeLocationCard dunwichVillage_243
    , SomeLocationCard coldSpringGlen_244
    , SomeLocationCard coldSpringGlen_245
    , SomeLocationCard tenAcreMeadow_246
    , SomeLocationCard tenAcreMeadow_247
    , SomeLocationCard blastedHeath_248
    , SomeLocationCard blastedHeath_249
    , SomeLocationCard whateleyRuins_250
    , SomeLocationCard whateleyRuins_251
    , SomeLocationCard devilsHopYard_252
    , SomeLocationCard devilsHopYard_253
    , -- Where Doom Awaits
      SomeLocationCard baseOfTheHill
    , SomeLocationCard ascendingPath
    , SomeLocationCard sentinelPeak
    , SomeLocationCard slaughteredWoods
    , SomeLocationCard eerieGlade
    , SomeLocationCard destroyedPath
    , SomeLocationCard frozenSpring
    , SomeLocationCard dimensionalGap
    , SomeLocationCard aTearInThePath
    , SomeLocationCard uprootedWoods
    , SomeLocationCard lostMemories
    , -- Lost in Time and Space
      SomeLocationCard anotherDimension
    , SomeLocationCard theEdgeOfTheUniverse
    , SomeLocationCard tearThroughTime
    , SomeLocationCard tearThroughSpace
    , SomeLocationCard prismaticCascade
    , SomeLocationCard endlessBridge
    , SomeLocationCard stepsOfYhagharl
    , SomeLocationCard dimensionalDoorway
    , -- The Path to Carcosa
      -- Curtain Call
      SomeLocationCard theatre
    , SomeLocationCard lobby
    , SomeLocationCard balcony
    , SomeLocationCard backstage
    , SomeLocationCard lightingBox
    , SomeLocationCard boxOffice
    , SomeLocationCard greenRoom
    , SomeLocationCard dressingRoom
    , SomeLocationCard rehearsalRoom
    , SomeLocationCard trapRoom
    , -- The Last King
      SomeLocationCard foyer
    , SomeLocationCard ballroom
    , SomeLocationCard livingRoom
    , SomeLocationCard gallery
    , SomeLocationCard courtyard
    , SomeLocationCard diningRoom
    , -- Echoes of the Past
      SomeLocationCard entryHall
    , SomeLocationCard historicalSocietyMeetingRoom
    , SomeLocationCard historicalSocietyRecordOffice_129
    , SomeLocationCard historicalSocietyHistoricalMuseum_130
    , SomeLocationCard quietHalls_131
    , SomeLocationCard historicalSocietyHistoricalMuseum_132
    , SomeLocationCard historicalSocietyHistoricalLibrary_133
    , SomeLocationCard historicalSocietyReadingRoom
    , SomeLocationCard quietHalls_135
    , SomeLocationCard historicalSocietyHistoricalLibrary_136
    , SomeLocationCard historicalSocietyPeabodysOffice
    , SomeLocationCard historicalSocietyRecordOffice_138
    , SomeLocationCard hiddenLibrary
    , -- The Unspeakable Oath
      SomeLocationCard asylumHallsWesternPatientWing_168
    , SomeLocationCard asylumHallsWesternPatientWing_169
    , SomeLocationCard asylumHallsEasternPatientWing_170
    , SomeLocationCard asylumHallsEasternPatientWing_171
    , SomeLocationCard kitchen
    , SomeLocationCard messHall
    , SomeLocationCard infirmary
    , SomeLocationCard yard
    , SomeLocationCard garden
    , SomeLocationCard basementHall
    , SomeLocationCard patientConfinementDanielsCell
    , SomeLocationCard patientConfinementOccupiedCell
    , SomeLocationCard patientConfinementDrearyCell
    , SomeLocationCard patientConfinementFamiliarCell
    , -- A Phantom of Truth
      SomeLocationCard montparnasse
    , SomeLocationCard montmartre209
    , SomeLocationCard montmartre210
    , SomeLocationCard grandGuignol
    , SomeLocationCard operaGarnier212
    , SomeLocationCard operaGarnier213
    , SomeLocationCard gareDOrsay
    , SomeLocationCard pereLachaiseCemetery
    , SomeLocationCard canalSaintMartin
    , SomeLocationCard leMarais217
    , SomeLocationCard leMarais218
    , SomeLocationCard notreDame
    , SomeLocationCard gardensOfLuxembourg
    , -- The Pallid Mask
      SomeLocationCard theGateToHell
    , SomeLocationCard stoneArchways
    , SomeLocationCard cryptOfTheSepulchralLamp
    , SomeLocationCard boneFilledCaverns
    , SomeLocationCard wellOfSouls
    , SomeLocationCard candlelitTunnels
    , SomeLocationCard labyrinthOfBones
    , SomeLocationCard narrowShaft
    , SomeLocationCard shiveringPools
    , SomeLocationCard blockedPassage
    , SomeLocationCard tombOfShadows
    , -- Black Stars Rise
      SomeLocationCard porteDeLAvancee
    , SomeLocationCard grandRue
    , SomeLocationCard outerWall_285
    , SomeLocationCard outerWall_286
    , SomeLocationCard northTower_287
    , SomeLocationCard northTower_288
    , SomeLocationCard brokenSteps_289
    , SomeLocationCard brokenSteps_290
    , SomeLocationCard abbeyChurch
    , SomeLocationCard choeurGothique_292
    , SomeLocationCard choeurGothique_293
    , SomeLocationCard cloister
    , SomeLocationCard knightsHall
    , SomeLocationCard chapelOfStAubertThePathIsOpen
    , SomeLocationCard chapelOfStAubertWatersForbidden
    , SomeLocationCard abbeyTowerThePathIsOpen
    , SomeLocationCard abbeyTowerSpiresForbidden
    , -- Dim Carcosa
      SomeLocationCard shoresOfHali
    , SomeLocationCard bleakPlainsStarsOfAldebaran
    , SomeLocationCard bleakPlainsBleakDesolation
    , SomeLocationCard ruinsOfCarcosaInhabitantOfCarcosa
    , SomeLocationCard ruinsOfCarcosaAMomentsRest
    , SomeLocationCard ruinsOfCarcosaTheCoffin
    , SomeLocationCard dimStreetsMappingTheStreets
    , SomeLocationCard dimStreetsTheKingsParade
    , SomeLocationCard dimStreetsTheArchway
    , SomeLocationCard depthsOfDemheTheHeightOfTheDepths
    , SomeLocationCard depthsOfDemheStepsOfThePalace
    , SomeLocationCard darkSpires
    , SomeLocationCard palaceOfTheKing
    , -- The Forgotten Age
      -- The Untamed Wilds
      SomeLocationCard expeditionCamp
    , SomeLocationCard ruinsOfEztli
    , -- The Doom of Eztli
      SomeLocationCard entryway
    , SomeLocationCard ancientHall
    , SomeLocationCard grandChamber
    , SomeLocationCard burialPit
    , SomeLocationCard undergroundRuins
    , SomeLocationCard secretPassage
    , SomeLocationCard chamberOfTime
    , -- Rainforest
      SomeLocationCard pathOfThorns
    , SomeLocationCard riverCanyon
    , SomeLocationCard ropeBridge
    , SomeLocationCard serpentsHaven
    , SomeLocationCard circuitousTrail
    , SomeLocationCard templeOfTheFang
    , SomeLocationCard overgrownRuins
    , -- Threads of Fate
      SomeLocationCard eztliExhibit
    , SomeLocationCard velmasDiner
    , SomeLocationCard curiositieShoppe
    , SomeLocationCard townHall
    , SomeLocationCard arkhamPoliceStation
    , SomeLocationCard trainTracks
    , SomeLocationCard blackCave
    , -- The Boundary Beyond
      SomeLocationCard templeRuins
    , SomeLocationCard metropolitanCathedral
    , SomeLocationCard chapultepecPark
    , SomeLocationCard zocalo
    , SomeLocationCard xochimilco
    , SomeLocationCard coyoacan
    , SomeLocationCard temploMayor_174
    , SomeLocationCard temploMayor_175
    , SomeLocationCard templesOfTenochtitlan_176
    , SomeLocationCard templesOfTenochtitlan_177
    , SomeLocationCard chapultepecHill_178
    , SomeLocationCard chapultepecHill_179
    , SomeLocationCard canalsOfTenochtitlan_180
    , SomeLocationCard canalsOfTenochtitlan_181
    , SomeLocationCard lakeXochimilco_182
    , SomeLocationCard lakeXochimilco_183
    , SomeLocationCard sacredWoods_184
    , SomeLocationCard sacredWoods_185
    , -- Heart of the Elders
      SomeLocationCard mouthOfKnYanTheCavernsMaw
    , SomeLocationCard mouthOfKnYanTheDepthsBelow
    , --- Pillars of Judgement
      SomeLocationCard timeWrackedWoods
    , SomeLocationCard stoneAltar
    , --- K'n-yan
      SomeLocationCard vastPassages
    , SomeLocationCard hallOfIdolatry
    , SomeLocationCard darkHollow
    , SomeLocationCard perilousGulch
    , SomeLocationCard crystalPillars
    , SomeLocationCard descentToYoth
    , -- The City of Ancients
      SomeLocationCard interviewRoomArrivalChamber
    , SomeLocationCard interviewRoomRestrainingChamber
    , SomeLocationCard interviewRoomIchorFilledChamber
    , SomeLocationCard hallsOfPnakotusNorthernCorridors
    , SomeLocationCard hallsOfPnakotusEasternCorridors
    , SomeLocationCard hallsOfPnakotusWesternCorridors
    , SomeLocationCard greatLibrary
    , SomeLocationCard yithianOrrery
    , SomeLocationCard laboratoryOfTheGreatRace
    , SomeLocationCard deconstructionRoom
    , SomeLocationCard towersOfPnakotus
    , -- The Depths of Yoth
      SomeLocationCard stepsOfYoth
    , SomeLocationCard cityOfTheSerpents
    , SomeLocationCard hallOfHeresy
    , SomeLocationCard crumblingPrecipice
    , SomeLocationCard cavernsOfYoth
    , SomeLocationCard forkedPath
    , SomeLocationCard bridgeOverNKai
    , SomeLocationCard brokenPassage
    , SomeLocationCard abandonedSite
    , SomeLocationCard brightCanyon
    , -- Shattered Aeons
      SomeLocationCard nexusOfNKai
    , SomeLocationCard yuggoth
    , SomeLocationCard shoresOfRlyeh
    , SomeLocationCard cityOfTheUnseen
    , SomeLocationCard aPocketInTime
    , SomeLocationCard ruinsOfNewYork
    , SomeLocationCard mu
    , SomeLocationCard atlantis
    , SomeLocationCard pnakotus
    , SomeLocationCard valusia
    , SomeLocationCard plateauOfLeng
    , --- Special
      SomeLocationCard betweenWorlds
    , -- The Circle Undone
      -- The Witching Hour
      SomeLocationCard witchesCircle
    , SomeLocationCard witchHauntedWoodsAbandonedMine
    , SomeLocationCard witchHauntedWoodsCairnStones
    , SomeLocationCard witchHauntedWoodsTheLonelyTree
    , SomeLocationCard witchHauntedWoodsChildsTreeHouse
    , SomeLocationCard witchHauntedWoodsTaintedWell
    , SomeLocationCard witchHauntedWoodsHermitsHouse
    , SomeLocationCard witchHauntedWoodsOvergrownBarn
    , -- At Death's Doorstep
      SomeLocationCard entryHallAtDeathsDoorstep
    , SomeLocationCard victorianHalls
    , SomeLocationCard trophyRoom
    , SomeLocationCard billiardsRoom
    , SomeLocationCard masterBedroom
    , SomeLocationCard balconyAtDeathsDoorstep
    , SomeLocationCard office
    , SomeLocationCard entryHallSpectral
    , SomeLocationCard victorianHallsSpectral
    , SomeLocationCard trophyRoomSpectral
    , SomeLocationCard billiardsRoomSpectral
    , SomeLocationCard masterBedroomSpectral
    , SomeLocationCard balconySpectral
    , SomeLocationCard officeSpectral
    , -- The Secret Name
      SomeLocationCard moldyHalls
    , SomeLocationCard landlordsQuarters
    , SomeLocationCard joeMazurewiczsRoom
    , SomeLocationCard frankElwoodsRoom
    , SomeLocationCard walterGilmansRoom
    , SomeLocationCard keziahsRoom
    , SomeLocationCard moldyHallsEarlierTonight
    , SomeLocationCard twilightAbyss
    , SomeLocationCard cityOfElderThings
    , SomeLocationCard witchHouseRuins
    , SomeLocationCard salemGaol1692
    , SomeLocationCard physicsClassroom
    , SomeLocationCard courtOfTheGreatOldOnesANotTooDistantFuture
    , SomeLocationCard siteOfTheSacrifice
    , SomeLocationCard strangeGeometry
    , -- The Wages of Sin
      SomeLocationCard hangmansBrook
    , SomeLocationCard hangmansBrookSpectral
    , SomeLocationCard hauntedFields
    , SomeLocationCard hauntedFieldsSpectral
    , SomeLocationCard abandonedChapel
    , SomeLocationCard abandonedChapelSpectral
    , SomeLocationCard theGallows_169
    , SomeLocationCard theGallowsSpectral_169
    , SomeLocationCard theGallows_170
    , SomeLocationCard theGallowsSpectral_170
    , SomeLocationCard hereticsGraves_171
    , SomeLocationCard hereticsGravesSpectral_171
    , SomeLocationCard hereticsGraves_172
    , SomeLocationCard hereticsGravesSpectral_172
    , SomeLocationCard chapelCrypt_173
    , SomeLocationCard chapelCryptSpectral_173
    , SomeLocationCard chapelCrypt_174
    , SomeLocationCard chapelCryptSpectral_174
    , SomeLocationCard chapelAttic_175
    , SomeLocationCard chapelAtticSpectral_175
    , SomeLocationCard chapelAttic_176
    , SomeLocationCard chapelAtticSpectral_176
    , -- The Wages of Sin
      SomeLocationCard lodgeGatesWeveBeenExpectingYou
    , SomeLocationCard lodgeGatesMembersOnly
    , SomeLocationCard lobbyWeveBeenExpectingYou
    , SomeLocationCard lobbyMembersOnly
    , SomeLocationCard lodgeCellarWeveBeenExpectingYou
    , SomeLocationCard lodgeCellarMembersOnly
    , SomeLocationCard lounge
    , SomeLocationCard vault
    , SomeLocationCard library
    , SomeLocationCard lodgeCatacombs
    , SomeLocationCard sanctumDoorwayCeremonyRoom
    , SomeLocationCard sanctumDoorwayHoldingCells
    , SomeLocationCard innerSanctum
    , -- Union and Disillusion
      SomeLocationCard miskatonicRiver
    , SomeLocationCard forbiddingShore
    , SomeLocationCard unvisitedIsleStandingStones
    , SomeLocationCard unvisitedIsleMistyClearing
    , SomeLocationCard unvisitedIsleForsakenWoods
    , SomeLocationCard unvisitedIsleMossCoveredSteps
    , SomeLocationCard unvisitedIsleHauntedSpring
    , SomeLocationCard unvisitedIsleDecayedWillow
    , SomeLocationCard theGeistTrap
    , -- In the Clutches of Chaos
      SomeLocationCard frenchHill_290
    , SomeLocationCard frenchHill_291
    , SomeLocationCard rivertown_292
    , SomeLocationCard rivertown_293
    , SomeLocationCard southside_294
    , SomeLocationCard southside_295
    , SomeLocationCard uptown_296
    , SomeLocationCard uptown_297
    , SomeLocationCard southChurch_298
    , SomeLocationCard southChurch_299
    , SomeLocationCard merchantDistrict_300
    , SomeLocationCard merchantDistrict_301
    , SomeLocationCard hangmansHillWhereItAllEnds
    , SomeLocationCard silverTwilightLodgeShroudedInMystery
    , SomeLocationCard hangmansHillShroudedInMystery
    , SomeLocationCard silverTwilightLodgeWhereItAllEnds
    , -- Before the Black Throne
      SomeLocationCard cosmicIngress
    , SomeLocationCard hideousPalace
    , SomeLocationCard courtOfTheGreatOldOnes
    , SomeLocationCard theBlackThrone
    , SomeLocationCard dancersMist
    , SomeLocationCard flightIntoOblivion
    , SomeLocationCard infinityOfDarkness
    , SomeLocationCard cosmicGate
    , SomeLocationCard pathwayIntoVoid
    , --- Special
      SomeLocationCard emptySpace
    , -- The Dream-Eaters
      -- signature
      SomeLocationCard dreamGateWondrousJourney
    , SomeLocationCard dreamGatePointlessReality
    , -- Beyond the Gates of Sleep
      SomeLocationCard seventySteps
    , SomeLocationCard theCavernOfFlame
    , SomeLocationCard sevenHundredSteps
    , SomeLocationCard baseOfTheSteps
    , SomeLocationCard theEnchantedPath
    , SomeLocationCard enchantedWoodsMysticalForest
    , SomeLocationCard enchantedWoodsVillageOfZoogs
    , SomeLocationCard enchantedWoodsGreatStoneCircle
    , SomeLocationCard enchantedWoodsStoneTrapdoor
    , SomeLocationCard enchantedWoodsTheMoonTree
    , SomeLocationCard enchantedWoodsFungalForest
    , SomeLocationCard enchantedWoodsLostWoods
    , -- Waking Nightmare
      SomeLocationCard waitingRoom
    , SomeLocationCard emergencyRoom
    , SomeLocationCard experimentalTherapiesWard
    , SomeLocationCard recordsOffice
    , SomeLocationCard stairwell
    , SomeLocationCard morgue
    , SomeLocationCard operatingRoom
    , SomeLocationCard privateRoom
    , --- The Search for Kadath
      SomeLocationCard ulthar
    , SomeLocationCard skaiRiver
    , SomeLocationCard dylathLeen
    , SomeLocationCard kadatheron
    , SomeLocationCard sarnath
    , SomeLocationCard ruinsOfIb
    , SomeLocationCard ilekVad
    , SomeLocationCard forbiddenLands
    , SomeLocationCard zulanThek
    , SomeLocationCard baharna
    , SomeLocationCard mtNgranek
    , SomeLocationCard namelessRuins
    , SomeLocationCard celephais
    , SomeLocationCard serannian
    , SomeLocationCard hazuthKleg
    , SomeLocationCard templeOfUnattainableDesires
    , SomeLocationCard cityWhichAppearsOnNoMap
    , -- A Thousand Shapes of Horror
      SomeLocationCard burialGround
    , SomeLocationCard frontPorchEntryway
    , SomeLocationCard downstairsDoorwayDen
    , SomeLocationCard downstairsDoorwayParlor
    , SomeLocationCard upstairsHallway
    , SomeLocationCard upstairsDoorwayLibrary
    , SomeLocationCard upstairsDoorwayBedroom
    , SomeLocationCard attic_AThousandShapesOfHorror
    , SomeLocationCard unmarkedTomb
    , SomeLocationCard mysteriousStairs_183
    , SomeLocationCard mysteriousStairs_184
    , SomeLocationCard mysteriousStairs_185
    , SomeLocationCard mysteriousStairs_186
    , SomeLocationCard mysteriousStairs_187
    , SomeLocationCard mysteriousStairs_188
    , -- Dark Side of the Moon
      SomeLocationCard moonBeastGalley
    , SomeLocationCard cityOfTheMoonBeasts
    , SomeLocationCard theDarkCrater
    , SomeLocationCard templeOfTheMoonLizard
    , SomeLocationCard moonForest
    , SomeLocationCard cavernsBeneathTheMoonDarkSide
    , SomeLocationCard theBlackCore
    , SomeLocationCard cavernsBeneathTheMoonLightSide
    , SomeLocationCard lightSideOfTheMoon
    , SomeLocationCard theWhiteShip
    , -- Point of No Return
      SomeLocationCard vaultsOfZin
    , SomeLocationCard cityOfGugs
    , SomeLocationCard towerOfKoth
    , SomeLocationCard plainOfTheGhouls
    , SomeLocationCard cragOfTheGhouls
    , SomeLocationCard seaOfBones
    , SomeLocationCard peaksOfThok
    , SomeLocationCard valeOfPnath
    , SomeLocationCard seaOfPitch_262
    , SomeLocationCard seaOfPitch_263
    , SomeLocationCard seaOfPitch_264
    , SomeLocationCard seaOfPitch_265
    , --- WhereTheGodsDwell
      SomeLocationCard plateauOfLengWhereTheGodsDwell
    , SomeLocationCard coldWastes
    , SomeLocationCard monasteryOfLeng
    , SomeLocationCard onyxGates
    , SomeLocationCard theOnyxCastle
    , SomeLocationCard forsakenTowerOfIllusionAndMyth
    , SomeLocationCard forsakenTowerOfLifeAndDeath
    , SomeLocationCard forsakenTowerOfInfiniteTruth
    , SomeLocationCard forsakenTowerOfEternalFlame
    , SomeLocationCard forsakenTowerOfTheQueenOfNight
    , SomeLocationCard forsakenTowerOfPrimevalLight
    , --- Weaver of the Cosmos
      SomeLocationCard theGreatWebWebStairs
    , SomeLocationCard theGreatWebCosmicWeb
    , SomeLocationCard theGreatWebTangledWeb
    , SomeLocationCard theGreatWebPrisonOfCocoons
    , SomeLocationCard theGreatWebVastWeb
    , SomeLocationCard theGreatWebWebWovenIsland
    , -- The Innsmouth Conspiracy
      --- The Pit of Despair
      SomeLocationCard unfamiliarChamber
    , SomeLocationCard boneRiddenPit
    , SomeLocationCard fishGraveyard
    , SomeLocationCard altarToDagon
    , SomeLocationCard idolChamber
    , SomeLocationCard sealedExit
    , --- The Vanishing of Elina Harper
      SomeLocationCard marshRefinery
    , SomeLocationCard gilmanHouse
    , SomeLocationCard innsmouthSquare
    , SomeLocationCard innsmouthHarbour
    , SomeLocationCard firstNationalGrocery
    , SomeLocationCard fishStreetBridge
    , SomeLocationCard theLittleBookshop
    , SomeLocationCard esotericOrderOfDagon
    , SomeLocationCard sawboneAlley
    , SomeLocationCard shorewardSlums
    , SomeLocationCard theHouseOnWaterStreet
    , SomeLocationCard innsmouthJail
    , SomeLocationCard newChurchGreen
    , --- Flooded Caverns
      SomeLocationCard underwaterCavern
    , SomeLocationCard tidalPool
    , SomeLocationCard undergroundRiver
    , --- In Too Deep [itp]
      SomeLocationCard esotericOrderOfDagonInTooDeep
    , SomeLocationCard sawboneAlleyInTooDeep
    , SomeLocationCard shorewardSlumsInTooDeep
    , SomeLocationCard theHouseOnWaterStreetInTooDeep
    , SomeLocationCard innsmouthJailInTooDeep
    , SomeLocationCard newChurchGreenInTooDeep
    , SomeLocationCard theLittleBookshopInTooDeep
    , SomeLocationCard fishStreetBridgeInTooDeep
    , SomeLocationCard firstNationalGroceryInTooDeep
    , SomeLocationCard innsmouthHarbourInTooDeep
    , SomeLocationCard innsmouthSquareInTooDeep
    , SomeLocationCard gilmanHouseInTooDeep
    , SomeLocationCard marshRefineryInTooDeep
    , SomeLocationCard railroadStation
    , SomeLocationCard desolateCoastline
    , --- Devil Reef [def]
      SomeLocationCard churningWaters
    , SomeLocationCard lonelyIsle
    , SomeLocationCard hiddenCove
    , SomeLocationCard wavewornIsland
    , SomeLocationCard saltMarshes
    , SomeLocationCard blackReef
    , SomeLocationCard bootleggersHideaway_174a
    , SomeLocationCard bootleggersHideaway_174b
    , SomeLocationCard deepOneGrotto_175a
    , SomeLocationCard deepOneGrotto_175b
    , SomeLocationCard cyclopeanRuins_176a
    , SomeLocationCard cyclopeanRuins_176b
    , SomeLocationCard templeOfTheUnion_177a
    , SomeLocationCard templeOfTheUnion_177b
    , --- Horror in High Gear [hhg]
      SomeLocationCard falconPointApproach
    , SomeLocationCard dimlyLitRoad_a
    , SomeLocationCard dimlyLitRoad_b
    , SomeLocationCard dimlyLitRoad_c
    , SomeLocationCard cliffsideRoad_a
    , SomeLocationCard cliffsideRoad_b
    , SomeLocationCard forkInTheRoad_a
    , SomeLocationCard forkInTheRoad_b
    , SomeLocationCard intersection_a
    , SomeLocationCard intersection_b
    , SomeLocationCard tightTurn_a
    , SomeLocationCard tightTurn_b
    , SomeLocationCard tightTurn_c
    , SomeLocationCard desolateRoad_a
    , SomeLocationCard desolateRoad_b
    , SomeLocationCard longWayAround
    , --- A Light in the Fog [lif]
      SomeLocationCard falconPointGatehouse
    , SomeLocationCard lighthouseStairwell
    , SomeLocationCard lanternRoom
    , SomeLocationCard falconPointCliffside
    , SomeLocationCard lighthouseKeepersCottage
    , SomeLocationCard sunkenGrottoUpperDepths
    , SomeLocationCard sunkenGrottoLowerDepths
    , SomeLocationCard sunkenGrottoFinalDepths
    , SomeLocationCard shrineToHydra
    , SomeLocationCard deepOneNursery
    , SomeLocationCard theMoonRoom
    , SomeLocationCard sunkenArchives
    , SomeLocationCard pumpRoom
    , SomeLocationCard holdingCells
    , --- The Lair of Dagon [lod]
      SomeLocationCard grandEntryway
    , SomeLocationCard hallOfBlood
    , SomeLocationCard hallOfTheDeep
    , SomeLocationCard foulCorridors
    , SomeLocationCard hallOfLoyalty
    , SomeLocationCard hallOfRebirth
    , SomeLocationCard hallOfSilence
    , SomeLocationCard doorwayToTheDepths
    , SomeLocationCard lairOfDagon
    , --- Into the Maelstrom [itm]
      SomeLocationCard darkAbyss
    , SomeLocationCard gatewayToYhanthlei
    , SomeLocationCard sunkenHalls
    , SomeLocationCard vaultOfRiches
    , SomeLocationCard underseaCorridors
    , SomeLocationCard statuesInTheDeep
    , SomeLocationCard submergedTemple
    , SomeLocationCard syzygyChamber
    , SomeLocationCard onyxGuardians
    , SomeLocationCard lairOfDagonIntoTheMaelstrom
    , SomeLocationCard lairOfHydra
    , -- Edge of the Earth
      --- Ice and Death
      SomeLocationCard crashSite
    , SomeLocationCard frozenShores
    , SomeLocationCard treacherousPath
    , SomeLocationCard precariousIceSheet
    , SomeLocationCard broadSnowdrifts
    , SomeLocationCard icyWastes
    , SomeLocationCard rockyCrags
    , SomeLocationCard snowGraves
    , SomeLocationCard icebreakerLanding
    , SomeLocationCard frigidCave
    , SomeLocationCard barrierCamp
    , SomeLocationCard remnantsOfLakesCamp
    , SomeLocationCard crystallineCavern
    , --- Fatal Mirage [eote]
      SomeLocationCard prisonOfMemories
    , SomeLocationCard baseCamp
    , SomeLocationCard deckOfTheTheodosia
    , SomeLocationCard universityHalls
    , SomeLocationCard hedgeMaze
    , SomeLocationCard desertedStation
    , SomeLocationCard coastalWaters
    , SomeLocationCard elderChamber
    , SomeLocationCard riverviewTheatre
    , SomeLocationCard standingStones
    , SomeLocationCard airfield
    , SomeLocationCard alaskanWilds
    , SomeLocationCard clutteredDormitory
    , SomeLocationCard dyersClassroom
    , SomeLocationCard infirmaryFatalMirage
    , SomeLocationCard drKenslersOffice
    , SomeLocationCard moaiStatues
    , SomeLocationCard ottomanFront
    , SomeLocationCard theBlackStone
    , --- To the Forbidden Peaks [eote]
      SomeLocationCard deepDrifts
    , SomeLocationCard whiteBluff
    , SomeLocationCard steepIncline
    , SomeLocationCard narrowRidge
    , SomeLocationCard rockyLedge
    , SomeLocationCard snowCoveredCrag
    , SomeLocationCard windsweptPath
    , SomeLocationCard theSummit
    , -- Return to Night of the Zealot
      -- Return to the Gathering
      SomeLocationCard studyAberrantGateway
    , SomeLocationCard guestHall
    , SomeLocationCard bedroom
    , SomeLocationCard bathroom
    , SomeLocationCard holeInTheWall
    , SomeLocationCard returnToAttic
    , SomeLocationCard farAboveYourHouse
    , SomeLocationCard returnToCellar
    , SomeLocationCard deepBelowYourHouse
    , -- Return to the Midnight Masks
      SomeLocationCard easttownArkhamPoliceStation
    , SomeLocationCard northsideTrainStation
    , SomeLocationCard miskatonicUniversityMiskatonicMuseum
    , SomeLocationCard rivertownAbandonedWarehouse
    , -- Return to the Devourer Below
      SomeLocationCard arkhamWoodsGreatWillow
    , SomeLocationCard arkhamWoodsLakeside
    , SomeLocationCard arkhamWoodsCorpseRiddenClearing
    , SomeLocationCard arkhamWoodsWoodenBridge
    , -- The Curse of the Rougarou
      SomeLocationCard cursedShores
    , SomeLocationCard gardenDistrict
    , SomeLocationCard broadmoor
    , SomeLocationCard brackishWaters
    , SomeLocationCard audubonPark
    , SomeLocationCard faubourgMarigny
    , SomeLocationCard forgottenMarsh
    , SomeLocationCard trappersCabin
    , SomeLocationCard twistedUnderbrush
    , SomeLocationCard foulSwamp
    , SomeLocationCard ritualGrounds
    , SomeLocationCard overgrownCairns
    , -- Carnevale of Horrors
      SomeLocationCard gondola
    , SomeLocationCard sanMarcoBasilica
    , SomeLocationCard canalSide
    , SomeLocationCard streetsOfVenice
    , SomeLocationCard rialtoBridge
    , SomeLocationCard venetianGarden
    , SomeLocationCard bridgeOfSighs
    , SomeLocationCard floodedSquare
    , SomeLocationCard accademiaBridge
    , SomeLocationCard theGuardian
    , -- Murder at the Excelsior Hotel
      SomeLocationCard room225
    , SomeLocationCard suiteBalcony
    , SomeLocationCard secondFloorHall
    , SomeLocationCard foyerMurderAtTheExcelsiorHotel
    , SomeLocationCard restaurant
    , SomeLocationCard hotelRoof
    , SomeLocationCard room212
    , SomeLocationCard room245
    , SomeLocationCard officeMurderAtTheExcelsiorHotel
    , SomeLocationCard basement
    ]
