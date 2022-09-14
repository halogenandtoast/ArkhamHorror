{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Location
  ( module Arkham.Location
  , module X
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Card.Id
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Location.Types as X ( Location )
import Arkham.Location.Locations
import Arkham.Location.Runner
import Arkham.Message
import Data.UUID ( nil )

createLocation :: IsCard a => a -> Location
createLocation a = lookupLocation (toCardCode a) (LocationId $ toCardId a)

lookupLocationStub :: CardCode -> Location
lookupLocationStub = ($ LocationId (CardId nil)) . lookupLocation

lookupLocation :: CardCode -> (LocationId -> Location)
lookupLocation lid = case lookup lid allLocations of
  Nothing -> error $ "Unknown locaiton: " <> show lid
  Just (SomeLocationCard a) -> Location <$> cbCardBuilder a

instance RunMessage Location where
  runMessage msg x@(Location l) = do
    modifiers' <- getModifiers (toTarget x)
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    Location <$> runMessage msg' l

instance FromJSON Location where
  parseJSON v = flip (withObject "Location") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withLocationCardCode cCode
      $ \(_ :: LocationCard a) -> Location <$> parseJSON @a v

withLocationCardCode
  :: CardCode -> (forall a . IsLocation a => LocationCard a -> r) -> r
withLocationCardCode cCode f = case lookup cCode allLocations of
  Nothing -> error "invalid locations"
  Just (SomeLocationCard a) -> f a

allLocations :: HashMap CardCode SomeLocationCard
allLocations = mapFromList $ map
  (toFst someLocationCardCode)
  [ -- Night of the Zealot
    -- The Gathering
    SomeLocationCard study
  , SomeLocationCard hallway
  , SomeLocationCard attic
  , SomeLocationCard cellar
  , SomeLocationCard parlor
  -- The Midnight Masks
  , SomeLocationCard yourHouse
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
  -- The Devourer Below
  , SomeLocationCard arkhamWoodsUnhallowedGround
  , SomeLocationCard arkhamWoodsTwistingPaths
  , SomeLocationCard arkhamWoodsOldHouse
  , SomeLocationCard arkhamWoodsCliffside
  , SomeLocationCard arkhamWoodsTangledThicket
  , SomeLocationCard arkhamWoodsQuietGlade
  , SomeLocationCard ritualSite
  -- The Dunwich Legacy
  -- Extracurricular Activity
  , SomeLocationCard miskatonicQuad
  , SomeLocationCard humanitiesBuilding
  , SomeLocationCard orneLibrary
  , SomeLocationCard studentUnion
  , SomeLocationCard dormitories
  , SomeLocationCard administrationBuilding
  , SomeLocationCard facultyOfficesTheNightIsStillYoung
  , SomeLocationCard facultyOfficesTheHourIsLate
  , SomeLocationCard scienceBuilding
  , SomeLocationCard alchemyLabs
  -- The House Always Wins
  , SomeLocationCard laBellaLuna
  , SomeLocationCard cloverClubLounge
  , SomeLocationCard cloverClubBar
  , SomeLocationCard cloverClubCardroom
  , SomeLocationCard darkenedHall
  , SomeLocationCard artGallery
  , SomeLocationCard vipArea
  , SomeLocationCard backAlley
  -- The Miskatonic Museum
  , SomeLocationCard museumEntrance
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
  -- The Essex County Express
  , SomeLocationCard passengerCar_167
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
  -- Blood on the Altar
  , SomeLocationCard villageCommons
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
  -- Undimensioned and Unseen
  , SomeLocationCard dunwichVillage_242
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
  -- Where Doom Awaits
  , SomeLocationCard baseOfTheHill
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
  -- Lost in Time and Space
  , SomeLocationCard anotherDimension
  , SomeLocationCard theEdgeOfTheUniverse
  , SomeLocationCard tearThroughTime
  , SomeLocationCard tearThroughSpace
  , SomeLocationCard prismaticCascade
  , SomeLocationCard endlessBridge
  , SomeLocationCard stepsOfYhagharl
  , SomeLocationCard dimensionalDoorway
  -- The Path to Carcosa
  -- Curtain Call
  , SomeLocationCard theatre
  , SomeLocationCard lobby
  , SomeLocationCard balcony
  , SomeLocationCard backstage
  , SomeLocationCard lightingBox
  , SomeLocationCard boxOffice
  , SomeLocationCard greenRoom
  , SomeLocationCard dressingRoom
  , SomeLocationCard rehearsalRoom
  , SomeLocationCard trapRoom
  -- The Last King
  , SomeLocationCard foyer
  , SomeLocationCard ballroom
  , SomeLocationCard livingRoom
  , SomeLocationCard gallery
  , SomeLocationCard courtyard
  , SomeLocationCard diningRoom
  -- Echoes of the Past
  , SomeLocationCard entryHall
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
  -- The Unspeakable Oath
  , SomeLocationCard asylumHallsWesternPatientWing_168
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
  -- A Phantom of Truth
  , SomeLocationCard montparnasse
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
  -- The Pallid Mask
  , SomeLocationCard theGateToHell
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
  -- Black Stars Rise
  , SomeLocationCard porteDeLAvancee
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
  -- Dim Carcosa
  , SomeLocationCard shoresOfHali
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
  -- The Forgotten Age
  -- The Untamed Wilds
  , SomeLocationCard expeditionCamp
  , SomeLocationCard ruinsOfEztli
  -- The Doom of Eztli
  , SomeLocationCard entryway
  , SomeLocationCard ancientHall
  , SomeLocationCard grandChamber
  , SomeLocationCard burialPit
  , SomeLocationCard undergroundRuins
  , SomeLocationCard secretPassage
  , SomeLocationCard chamberOfTime
  -- Rainforest
  , SomeLocationCard pathOfThorns
  , SomeLocationCard riverCanyon
  , SomeLocationCard ropeBridge
  , SomeLocationCard serpentsHaven
  , SomeLocationCard circuitousTrail
  , SomeLocationCard templeOfTheFang
  , SomeLocationCard overgrownRuins
  -- Threads of Fate
  , SomeLocationCard eztliExhibit
  , SomeLocationCard velmasDiner
  , SomeLocationCard curiositieShoppe
  , SomeLocationCard townHall
  , SomeLocationCard arkhamPoliceStation
  , SomeLocationCard trainTracks
  -- Return to Night of the Zealot
  -- Return to the Gathering
  , SomeLocationCard studyAberrantGateway
  , SomeLocationCard guestHall
  , SomeLocationCard bedroom
  , SomeLocationCard bathroom
  , SomeLocationCard holeInTheWall
  , SomeLocationCard returnToAttic
  , SomeLocationCard farAboveYourHouse
  , SomeLocationCard returnToCellar
  , SomeLocationCard deepBelowYourHouse
  -- Return to the Midnight Masks
  , SomeLocationCard easttownArkhamPoliceStation
  , SomeLocationCard northsideTrainStation
  , SomeLocationCard miskatonicUniversityMiskatonicMuseum
  , SomeLocationCard rivertownAbandonedWarehouse
  -- Return to the Devourer Below
  , SomeLocationCard arkhamWoodsGreatWillow
  , SomeLocationCard arkhamWoodsLakeside
  , SomeLocationCard arkhamWoodsCorpseRiddenClearing
  , SomeLocationCard arkhamWoodsWoodenBridge
  -- The Curse of the Rougarou
  , SomeLocationCard cursedShores
  , SomeLocationCard gardenDistrict
  , SomeLocationCard broadmoor
  , SomeLocationCard brackishWaters
  , SomeLocationCard audubonPark
  , SomeLocationCard fauborgMarigny
  , SomeLocationCard forgottenMarsh
  , SomeLocationCard trappersCabin
  , SomeLocationCard twistedUnderbrush
  , SomeLocationCard foulSwamp
  , SomeLocationCard ritualGrounds
  , SomeLocationCard overgrownCairns
  -- Carnevale of Horrors
  , SomeLocationCard gondola
  , SomeLocationCard sanMarcoBasilica
  , SomeLocationCard canalSide
  , SomeLocationCard streetsOfVenice
  , SomeLocationCard rialtoBridge
  , SomeLocationCard venetianGarden
  , SomeLocationCard bridgeOfSighs
  , SomeLocationCard floodedSquare
  , SomeLocationCard accademiaBridge
  , SomeLocationCard theGuardian
  ]

