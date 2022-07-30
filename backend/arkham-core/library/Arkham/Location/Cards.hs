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
  -> CardDef
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
    , cdLocationRevealedSymbol = locationSymbol
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
location cardCode name traits encounterSet locationSymbol connectedLocationSymbols
  = CardDef
    { cdCardCode = cardCode
    , cdName = name
    , cdRevealedName = Just name
    , cdCost = Nothing
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
    , cdLocationSymbol = locationSymbol
    , cdLocationRevealedSymbol = locationSymbol
    , cdLocationConnections = connectedLocationSymbols
    , cdLocationRevealedConnections = connectedLocationSymbols
    }

allLocationCards :: HashMap CardCode CardDef
allLocationCards = mapFromList $ map
  (toCardCode &&& id)
  [ aTearInThePath
  , abbeyChurch
  , abbeyTowerSpiresForbidden
  , abbeyTowerThePathIsOpen
  , accademiaBridge
  , administrationBuilding
  , administrationOffice_130
  , administrationOffice_131
  , alchemyLabs
  , anotherDimension
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
  , blastedHeath_248
  , blastedHeath_249
  , bleakPlainsBleakDesolation
  , bleakPlainsStarsOfAldebaran
  , blockedPassage
  , boneFilledCaverns
  , boxOffice
  , brackishWaters
  , bridgeOfSighs
  , broadmoor
  , brokenSteps_289
  , brokenSteps_290
  , burnedRuins_204
  , burnedRuins_205
  , canalSaintMartin
  , canalSide
  , candlelitTunnels
  , cellar
  , chapelOfStAubertThePathIsOpen
  , chapelOfStAubertWatersForbidden
  , choeurGothique_292
  , choeurGothique_293
  , circuitousTrail
  , cloister
  , cloverClubBar
  , cloverClubCardroom
  , cloverClubLounge
  , coldSpringGlen_244
  , coldSpringGlen_245
  , congregationalChurch_208
  , congregationalChurch_209
  , courtyard
  , cryptOfTheSepulchralLamp
  , cursedShores
  , darkenedHall
  , darkSpires
  , deepBelowYourHouse
  , depthsOfDemheTheHeightOfTheDepths
  , depthsOfDemheStepsOfThePalace
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
  , exhibitHallAthabaskanExhibit
  , exhibitHallEgyptianExhibit
  , exhibitHallHallOfTheDead
  , exhibitHallMedusaExhibit
  , exhibitHallNatureExhibit
  , exhibitHallRestrictedHall
  , expeditionCamp
  , facultyOfficesTheHourIsLate
  , facultyOfficesTheNightIsStillYoung
  , farAboveYourHouse
  , fauborgMarigny
  , floodedSquare
  , forgottenMarsh
  , foulSwamp
  , foyer
  , frozenSpring
  , gallery
  , garden
  , gardenDistrict
  , gardensOfLuxembourg
  , gareDOrsay
  , gondola
  , grandGuignol
  , grandRue
  , graveyard
  , greenRoom
  , guestHall
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
  , kitchen
  , knightsHall
  , laBellaLuna
  , labyrinthOfBones
  , leMarais217
  , leMarais218
  , lightingBox
  , livingRoom
  , lobby
  , lostMemories
  , mainPath
  , messHall
  , miskatonicQuad
  , miskatonicUniversity
  , miskatonicUniversityMiskatonicMuseum
  , montmartre209
  , montmartre210
  , montparnasse
  , museumEntrance
  , museumHalls
  , narrowShaft
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
  , sanMarcoBasilica
  , schoolhouse_212
  , schoolhouse_213
  , scienceBuilding
  , securityOffice_128
  , securityOffice_129
  , sentinelPeak
  , serpentsHaven
  , shiveringPools
  , shoresOfHali
  , slaughteredWoods
  , sleepingCar
  , southsideHistoricalSociety
  , southsideMasBoardingHouse
  , stMarysHospital
  , stepsOfYhagharl
  , stoneArchways
  , streetsOfVenice
  , studentUnion
  , study
  , studyAberrantGateway
  , tearThroughSpace
  , tearThroughTime
  , templeOfTheFang
  , tenAcreMeadow_246
  , tenAcreMeadow_247
  , theEdgeOfTheUniverse
  , theGateToHell
  , theGuardian
  , theHiddenChamber
  , theatre
  , tombOfShadows
  , trapRoom
  , trappersCabin
  , twistedUnderbrush
  , uprootedWoods
  , venetianGarden
  , villageCommons
  , vipArea
  , wellOfSouls
  , whateleyRuins_250
  , whateleyRuins_251
  , yard
  , yourHouse
  ]

study :: CardDef
study = location "01111" "Study" mempty Circle [] TheGathering

hallway :: CardDef
hallway = location
  "01112"
  "Hallway"
  mempty
  Square
  [Triangle, Plus, Diamond]
  TheGathering

attic :: CardDef
attic = (location "01113" "Attic" mempty Triangle [Square] TheGathering)
  { cdVictoryPoints = Just 1
  }

cellar :: CardDef
cellar = (location "01114" "Cellar" mempty Plus [Square] TheGathering)
  { cdVictoryPoints = Just 1
  }

parlor :: CardDef
parlor = location "01115" "Parlor" mempty Diamond [Square] TheGathering

yourHouse :: CardDef
yourHouse =
  location "01124" "Your House" [Arkham] Squiggle [Circle] TheMidnightMasks

rivertown :: CardDef
rivertown = location
  "01125"
  "Rivertown"
  [Arkham, Central]
  Circle
  [Moon, Diamond, Square, Squiggle, Hourglass]
  TheMidnightMasks

southsideHistoricalSociety :: CardDef
southsideHistoricalSociety = location
  "01126"
  ("Southside" <:> "Historical Society")
  [Arkham]
  Square
  [Triangle, Plus, Circle]
  TheMidnightMasks

southsideMasBoardingHouse :: CardDef
southsideMasBoardingHouse = location
  "01127"
  ("Southside" <:> "Ma's Boarding House")
  [Arkham]
  Square
  [Triangle, Plus, Circle]
  TheMidnightMasks

stMarysHospital :: CardDef
stMarysHospital = location
  "01128"
  "St. Mary's Hospital"
  [Arkham]
  Plus
  [Diamond, Square]
  TheMidnightMasks

miskatonicUniversity :: CardDef
miskatonicUniversity = (location
                         "01129"
                         "Miskatonic University"
                         [Arkham]
                         Diamond
                         [T, Plus, Circle, Square]
                         TheMidnightMasks
                       )
  { cdVictoryPoints = Just 1
  }

downtownFirstBankOfArkham :: CardDef
downtownFirstBankOfArkham = location
  "01130"
  ("Downtown" <:> "First Bank of Arkham")
  [Arkham]
  Triangle
  [Moon, T]
  TheMidnightMasks

downtownArkhamAsylum :: CardDef
downtownArkhamAsylum = (location
                         "01131"
                         ("Downtown" <:> "Arkham Asylum")
                         [Arkham]
                         Triangle
                         [Moon, T]
                         TheMidnightMasks
                       )
  { cdVictoryPoints = Just 1
  }

easttown :: CardDef
easttown =
  location "01132" "Easttown" [Arkham] Moon [Circle, Triangle] TheMidnightMasks

graveyard :: CardDef
graveyard =
  (location "01133" "Graveyard" [Arkham] Hourglass [Circle] TheMidnightMasks)
    { cdVictoryPoints = Just 1
    }

northside :: CardDef
northside =
  (location "01134" "Northside" [Arkham] T [Diamond, Triangle] TheMidnightMasks)
    { cdVictoryPoints = Just 1
    }

mainPath :: CardDef
mainPath =
  location "01149" "Main Path" [Woods] Squiggle [Square, Plus] TheDevourerBelow

arkhamWoodsUnhallowedGround :: CardDef
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

arkhamWoodsTwistingPaths :: CardDef
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

arkhamWoodsOldHouse :: CardDef
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

arkhamWoodsCliffside :: CardDef
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

arkhamWoodsTangledThicket :: CardDef
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

arkhamWoodsQuietGlade :: CardDef
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

ritualSite :: CardDef
ritualSite =
  location "01156" "Ritual Site" [Cave] Plus [Squiggle] TheDevourerBelow

miskatonicQuad :: CardDef
miskatonicQuad =
  location "02048" "Miskatonic Quad" [Miskatonic] Plus [Triangle, Hourglass, Square, Diamond, Circle] ExtracurricularActivity

humanitiesBuilding :: CardDef
humanitiesBuilding =
  location "02049" "Humanities Building" [Miskatonic] Square [Plus, Triangle] ExtracurricularActivity

orneLibrary :: CardDef
orneLibrary =
  (location "02050" "Orne Library" [Miskatonic] Triangle [Plus, Square] ExtracurricularActivity)
    { cdVictoryPoints = Just 1
    }

studentUnion :: CardDef
studentUnion =
  location "02051" "Student Union" [Miskatonic] Diamond [Plus, Equals] ExtracurricularActivity

dormitories :: CardDef
dormitories =
  (location "02052" "Dormitories" [Miskatonic] Equals [Diamond] ExtracurricularActivity)
    { cdVictoryPoints = Just 1
    }

administrationBuilding :: CardDef
administrationBuilding = location
  "02053"
  "Administration Building"
  [Miskatonic]
  Circle
  [Plus, T]
  ExtracurricularActivity

facultyOfficesTheNightIsStillYoung :: CardDef
facultyOfficesTheNightIsStillYoung = (location
                                       "02054"
                                       ("Faculty Offices"
                                       <:> "The Night is Still Young"
                                       )
                                       [Miskatonic]
                                       T
                                       [Circle]
                                       ExtracurricularActivity
                                     )
  { cdVictoryPoints = Just 1
  }

facultyOfficesTheHourIsLate :: CardDef
facultyOfficesTheHourIsLate = location
  "02055"
  ("Faculty Offices" <:> "The Hour is Late")
  [Miskatonic]
  T
  [Circle]
  ExtracurricularActivity

scienceBuilding :: CardDef
scienceBuilding =
  location "02056" "Science Building" [Miskatonic] Hourglass [Plus, Squiggle] ExtracurricularActivity

alchemyLabs :: CardDef
alchemyLabs =
  location "02057" "Alchemy Labs" [Miskatonic] Squiggle [Hourglass] ExtracurricularActivity

laBellaLuna :: CardDef
laBellaLuna = location "02070" "La Bella Luna" [Arkham] Moon [Circle] TheHouseAlwaysWins

cloverClubLounge :: CardDef
cloverClubLounge =
  location "02071" "Clover Club Lounge" [CloverClub] Circle [Moon, Square, Triangle] TheHouseAlwaysWins

cloverClubBar :: CardDef
cloverClubBar =
  location "02072" "Clover Club Bar" [CloverClub] Square [Triangle, Circle] TheHouseAlwaysWins

cloverClubCardroom :: CardDef
cloverClubCardroom =
  location "02073" "Clover Club Cardroom" [CloverClub] Triangle [Circle, Square, Diamond] TheHouseAlwaysWins

darkenedHall :: CardDef
darkenedHall = location "02074" "Darkened Hall" [CloverClub] Diamond [Triangle, T, Hourglass, Plus, Squiggle] TheHouseAlwaysWins

artGallery :: CardDef
artGallery = (locationWithUnrevealed
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
             )
  { cdVictoryPoints = Just 1
  }

vipArea :: CardDef
vipArea = (locationWithUnrevealed
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
          )
  { cdVictoryPoints = Just 1
  }

backAlley :: CardDef
backAlley = (locationWithUnrevealed
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
            )
  { cdVictoryPoints = Just 1
  }

museumEntrance :: CardDef
museumEntrance =
  location "02126" "Museum Entrance" [Miskatonic] Circle [Square] TheMiskatonicMuseum

museumHalls :: CardDef
museumHalls = location "02127" "Museum Halls" [Miskatonic] Square [Circle, Diamond, Triangle] TheMiskatonicMuseum

securityOffice_128 :: CardDef
securityOffice_128 =
  location "02128" "Security Office" [Miskatonic] Diamond [Square] TheMiskatonicMuseum

securityOffice_129 :: CardDef
securityOffice_129 =
  location "02129" "Security Office" [Miskatonic] Diamond [Square] TheMiskatonicMuseum

administrationOffice_130 :: CardDef
administrationOffice_130 =
  location "02130" "Administration Office" [Miskatonic] Triangle [Square] TheMiskatonicMuseum

administrationOffice_131 :: CardDef
administrationOffice_131 =
  location "02131" "Administration Office" [Miskatonic] Triangle [Square] TheMiskatonicMuseum

exhibitHallAthabaskanExhibit :: CardDef
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

exhibitHallMedusaExhibit :: CardDef
exhibitHallMedusaExhibit = (locationWithUnrevealed
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
                           )
  { cdVictoryPoints = Just 1
  }

exhibitHallNatureExhibit :: CardDef
exhibitHallNatureExhibit = (locationWithUnrevealed
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
                           )
  { cdVictoryPoints = Just 1
  }

exhibitHallEgyptianExhibit :: CardDef
exhibitHallEgyptianExhibit = (locationWithUnrevealed
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
                             )
  { cdVictoryPoints = Just 1
  }

exhibitHallHallOfTheDead :: CardDef
exhibitHallHallOfTheDead = (locationWithUnrevealed
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
                           )
  { cdVictoryPoints = Just 1
  }

exhibitHallRestrictedHall :: CardDef
exhibitHallRestrictedHall = (locationWithUnrevealed
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
                            )
  { cdVictoryPoints = Just 1
  }

passengerCar_167 :: CardDef
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

passengerCar_168 :: CardDef
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

passengerCar_169 :: CardDef
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

passengerCar_170 :: CardDef
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

passengerCar_171 :: CardDef
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

sleepingCar :: CardDef
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

diningCar :: CardDef
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

parlorCar :: CardDef
parlorCar = (locationWithUnrevealed
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
            )
  { cdVictoryPoints = Just 1
  }

engineCar_175 :: CardDef
engineCar_175 = (location "02175" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress)
  { cdVictoryPoints = Just 1
  }

engineCar_176 :: CardDef
engineCar_176 = (location "02176" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress)
  { cdVictoryPoints = Just 1
  }

engineCar_177 :: CardDef
engineCar_177 = (location "02177" "Engine Car" [Train] NoSymbol [] TheEssexCountyExpress)
  { cdVictoryPoints = Just 1
  }

villageCommons :: CardDef
villageCommons =
  location "02201" "Village Commons" [Dunwich, Central] Plus [Square, Circle, Moon] BloodOnTheAltar

bishopsBrook_202 :: CardDef
bishopsBrook_202 = location "02202" "Bishop's Brook" [Dunwich] Square [Plus, Circle, Triangle] BloodOnTheAltar

bishopsBrook_203 :: CardDef
bishopsBrook_203 = location "02203" "Bishop's Brook" [Dunwich] Square [Plus, Circle, Triangle] BloodOnTheAltar

burnedRuins_204 :: CardDef
burnedRuins_204 = location "02204" "Burned Ruins" [Dunwich] Triangle [Square, Diamond] BloodOnTheAltar

burnedRuins_205 :: CardDef
burnedRuins_205 = location "02205" "Burned Ruins" [Dunwich] Triangle [Square, Diamond] BloodOnTheAltar

osbornsGeneralStore_206 :: CardDef
osbornsGeneralStore_206 =
  location "02206" "Osborn's General Store" [Dunwich] Circle [Moon, Square] BloodOnTheAltar

osbornsGeneralStore_207 :: CardDef
osbornsGeneralStore_207 =
  location "02207" "Osborn's General Store" [Dunwich] Circle [Moon, Square] BloodOnTheAltar

congregationalChurch_208 :: CardDef
congregationalChurch_208 =
  location "02208" "Congregational Church" [Dunwich] Diamond [Plus, Triangle, Squiggle] BloodOnTheAltar

congregationalChurch_209 :: CardDef
congregationalChurch_209 =
  location "02209" "Congregational Church" [Dunwich] Diamond [Plus, Triangle, Squiggle] BloodOnTheAltar

houseInTheReeds_210 :: CardDef
houseInTheReeds_210 =
  location "02210" "House in the Reeds" [Dunwich] Squiggle [Diamond, Moon] BloodOnTheAltar

houseInTheReeds_211 :: CardDef
houseInTheReeds_211 =
  location "02211" "House in the Reeds" [Dunwich] Squiggle [Diamond, Moon] BloodOnTheAltar

schoolhouse_212 :: CardDef
schoolhouse_212 = location "02212" "Schoolhouse" [Dunwich] Moon [Plus, Squiggle, Moon] BloodOnTheAltar

schoolhouse_213 :: CardDef
schoolhouse_213 = location "02213" "Schoolhouse" [Dunwich] Moon [Plus, Squiggle, Moon] BloodOnTheAltar

theHiddenChamber :: CardDef
theHiddenChamber = (location
                     "02214"
                     ("The Hidden Chamber" <:> "Prison of the Beast")
                     [Dunwich]
                     NoSymbol
                     []
                     BloodOnTheAltar
                   )
  { cdVictoryPoints = Just 2
  }

dunwichVillage_242 :: CardDef
dunwichVillage_242 =
  location "02242" "Dunwich Village" [Dunwich] Circle [Triangle, Square, Diamond] UndimensionedAndUnseen

dunwichVillage_243 :: CardDef
dunwichVillage_243 =
  location "02243" "Dunwich Village" [Dunwich] Circle [Triangle, Square, Diamond] UndimensionedAndUnseen

coldSpringGlen_244 :: CardDef
coldSpringGlen_244 =
  location "02244" "Cold Spring Glen" [Dunwich] Triangle [Circle, Diamond, Plus] UndimensionedAndUnseen

coldSpringGlen_245 :: CardDef
coldSpringGlen_245 =
  location "02245" "Cold Spring Glen" [Dunwich] Triangle [Circle, Diamond, Plus] UndimensionedAndUnseen

tenAcreMeadow_246 :: CardDef
tenAcreMeadow_246 =
  location "02246" "Ten-Acre Meadow" [Dunwich] Diamond [Circle, Triangle, Plus] UndimensionedAndUnseen

tenAcreMeadow_247 :: CardDef
tenAcreMeadow_247 =
  location "02247" "Ten-Acre Meadow" [Dunwich] Diamond [Circle, Triangle, Plus] UndimensionedAndUnseen

blastedHeath_248 :: CardDef
blastedHeath_248 =
  location "02248" "Blasted Heath" [Dunwich] Square [Circle, Hourglass] UndimensionedAndUnseen

blastedHeath_249 :: CardDef
blastedHeath_249 =
  location "02249" "Blasted Heath" [Dunwich] Square [Circle, Hourglass] UndimensionedAndUnseen

whateleyRuins_250 :: CardDef
whateleyRuins_250 =
  location "02250" "Whateley Ruins" [Dunwich] Plus [Triangle, Diamond, Hourglass] UndimensionedAndUnseen

whateleyRuins_251 :: CardDef
whateleyRuins_251 =
  location "02251" "Whateley Ruins" [Dunwich] Plus [Triangle, Diamond, Hourglass] UndimensionedAndUnseen

devilsHopYard_252 :: CardDef
devilsHopYard_252 =
  location "02252" "Devil's Hop Yard" [Dunwich] Hourglass [Square, Plus] UndimensionedAndUnseen

devilsHopYard_253 :: CardDef
devilsHopYard_253 =
  location "02253" "Devil's Hop Yard" [Dunwich] Hourglass [Square, Plus] UndimensionedAndUnseen

baseOfTheHill :: CardDef
baseOfTheHill =
  location "02282" "Base of the Hill" [Dunwich, SentinelHill] Triangle [Square, Plus, Squiggle, Hourglass] WhereDoomAwaits

ascendingPath :: CardDef
ascendingPath =
  location "02283" "Ascending Path" [Dunwich, SentinelHill] Square [Triangle, Diamond, T, Equals, Moon] WhereDoomAwaits

sentinelPeak :: CardDef
sentinelPeak =
  (location "02284" "Sentinel Peak" [Dunwich, SentinelHill] Diamond [Square] WhereDoomAwaits)
    { cdVictoryPoints = Just 2
    }

slaughteredWoods :: CardDef
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

eerieGlade :: CardDef
eerieGlade = locationWithUnrevealed
  "02286"
  "Diverging Path"
  [Dunwich, Woods]
  NoSymbol
  []
  "Eerie Glade"
  Hourglass
  [Triangle, Plus]
  [Dunwich, Woods]
  WhereDoomAwaits

destroyedPath :: CardDef
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

frozenSpring :: CardDef
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

dimensionalGap :: CardDef
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

aTearInThePath :: CardDef
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

uprootedWoods :: CardDef
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

lostMemories :: CardDef
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

anotherDimension :: CardDef
anotherDimension = location
  "02320"
  ("Another Dimension" <:> "Unfettered by Reality")
  [Otherworld]
  Circle
  [Square, Diamond, Triangle]
  LostInTimeAndSpace

theEdgeOfTheUniverse :: CardDef
theEdgeOfTheUniverse =
  location "02321" "The Edge of the Universe" [Otherworld] Moon [Plus, Squiggle] LostInTimeAndSpace

tearThroughTime :: CardDef
tearThroughTime =
  location "02322" "Tear Through Time" [Otherworld] Moon [Circle, Plus, Squiggle] LostInTimeAndSpace

tearThroughSpace :: CardDef
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

prismaticCascade :: CardDef
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

endlessBridge :: CardDef
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

stepsOfYhagharl :: CardDef
stepsOfYhagharl = (location
                    "02327"
                    "Steps of Y'hagharl"
                    [Otherworld, Extradimensional]
                    Plus
                    [Diamond, Moon]
                    LostInTimeAndSpace
                  )
  { cdDoubleSided = False
  }

dimensionalDoorway :: CardDef
dimensionalDoorway = (location
                       "02328"
                       "Dimensional Doorway"
                       [Otherworld, Extradimensional]
                       Squiggle
                       [Triangle, Moon]
                       LostInTimeAndSpace
                     )
  { cdDoubleSided = False
  }

theatre :: CardDef
theatre = location "03049" "Theatre" mempty Circle [Diamond, Triangle] CurtainCall

lobby :: CardDef
lobby = location "03050" "Lobby" mempty Triangle [Circle, Squire, Plus] CurtainCall

balcony :: CardDef
balcony =
  (location "03051" "Balcony" mempty Square [Circle, Triangle] CurtainCall) { cdVictoryPoints = Just 1 }

backstage :: CardDef
backstage = location "03052" "Backstage" mempty Diamond [Circle, Moon] CurtainCall

lightingBox :: CardDef
lightingBox = (locationWithUnrevealed
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
              )
  { cdVictoryPoints = Just 1
  }

boxOffice :: CardDef
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

greenRoom :: CardDef
greenRoom = (locationWithUnrevealed
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
            )
  { cdVictoryPoints = Just 1
  }

dressingRoom :: CardDef
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

rehearsalRoom :: CardDef
rehearsalRoom = (locationWithUnrevealed
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
                )
  { cdVictoryPoints = Just 1
  }

trapRoom :: CardDef
trapRoom = (locationWithUnrevealed
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
           )
  { cdVictoryPoints = Just 1
  }

foyer :: CardDef
foyer = location "03070" "Foyer" mempty T [Circle, Square, Equals] TheLastKing

ballroom :: CardDef
ballroom = location "03071" "Ballroom" mempty Square [T, Circle, Squiggle] TheLastKing

livingRoom :: CardDef
livingRoom = location "03072" "Living Room" mempty Equals [T, Circle, Plus] TheLastKing

gallery :: CardDef
gallery = location "03073" "Gallery" mempty Plus [Equals, Circle] TheLastKing

courtyard :: CardDef
courtyard = location "03074" "Courtyard" mempty Circle [Squiggle, Square, T, Equals, Plus] TheLastKing

diningRoom :: CardDef
diningRoom = location "03075" "Dining Room" mempty Squiggle [Square, Circle] TheLastKing

entryHall :: CardDef
entryHall = location "03127" "Entry Hall" [GroundFloor] Square [Circle] EchoesOfThePast

historicalSocietyMeetingRoom :: CardDef
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

historicalSocietyRecordOffice_129 :: CardDef
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

historicalSocietyHistoricalMuseum_130 :: CardDef
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

quietHalls_131 :: CardDef
quietHalls_131 = location "03131" "Quiet Halls" [SecondFloor] Circle [Square, Star] EchoesOfThePast

historicalSocietyHistoricalMuseum_132 :: CardDef
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

historicalSocietyHistoricalLibrary_133 :: CardDef
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

historicalSocietyReadingRoom :: CardDef
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

quietHalls_135 :: CardDef
quietHalls_135 = location "03135" "Quiet Halls" [ThirdFloor] Star [Circle] EchoesOfThePast

historicalSocietyHistoricalLibrary_136 :: CardDef
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

historicalSocietyPeabodysOffice :: CardDef
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

historicalSocietyRecordOffice_138 :: CardDef
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

hiddenLibrary :: CardDef
hiddenLibrary = (location "03139" "Hidden Library" mempty NoSymbol [] EchoesOfThePast)
  { cdVictoryPoints = Just 2
  }

asylumHallsWesternPatientWing_168 :: CardDef
asylumHallsWesternPatientWing_168 = location
  "03168"
  ("Asylum Halls" <:> "Western Patient Wing")
  [ArkhamAsylum]
  Circle
  [Hourglass, Triangle, Diamond]
  TheUnspeakableOath

asylumHallsWesternPatientWing_169 :: CardDef
asylumHallsWesternPatientWing_169 = location
  "03169"
  ("Asylum Halls" <:> "Western Patient Wing")
  [ArkhamAsylum]
  Circle
  [Hourglass, Triangle, Diamond]
  TheUnspeakableOath

asylumHallsEasternPatientWing_170 :: CardDef
asylumHallsEasternPatientWing_170 = location
  "03170"
  ("Asylum Halls" <:> "Eastern Patient Wing")
  [ArkhamAsylum]
  Hourglass
  [Circle, Heart, Squiggle]
  TheUnspeakableOath

asylumHallsEasternPatientWing_171 :: CardDef
asylumHallsEasternPatientWing_171 = location
  "03171"
  ("Asylum Halls" <:> "Eastern Patient Wing")
  [ArkhamAsylum]
  Hourglass
  [Circle, Heart, Squiggle]
  TheUnspeakableOath

kitchen :: CardDef
kitchen = location "03172" "Kitchen" [ArkhamAsylum] Square [Triangle] TheUnspeakableOath

messHall :: CardDef
messHall = (location "03173" "Mess Hall" [ArkhamAsylum] Triangle [Circle, Square] TheUnspeakableOath)
  { cdVictoryPoints = Just 1
  }

infirmary :: CardDef
infirmary = (location "03174" "Infirmary" [ArkhamAsylum] Heart [Hourglass] TheUnspeakableOath)
  { cdVictoryPoints = Just 1
  }

yard :: CardDef
yard = location "03175" "Yard" [ArkhamAsylum] Diamond [Circle, Plus] TheUnspeakableOath

garden :: CardDef
garden = location "03176" "Garden" [ArkhamAsylum] Plus [Diamond] TheUnspeakableOath

basementHall :: CardDef
basementHall =
  (location "03177" "Basement Hall" [ArkhamAsylum] Squiggle [Hourglass, Moon] TheUnspeakableOath)
    { cdVictoryPoints = Just 1
    }

patientConfinementDanielsCell :: CardDef
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

patientConfinementOccupiedCell :: CardDef
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

patientConfinementDrearyCell :: CardDef
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

patientConfinementFamiliarCell :: CardDef
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

montparnasse :: CardDef
montparnasse = location "03208" "Montparnasse" [Paris, Rail] Circle [Heart, Star, Plus] APhantomOfTruth

montmartre209 :: CardDef
montmartre209 = location "03209" "Montmartre" [Paris, Rail] Square [Diamond, Triangle, Equals, Moon] APhantomOfTruth

montmartre210 :: CardDef
montmartre210 = location "03210" "Montmartre" [Paris, Rail] Square [Diamond, Triangle, Equals, Moon] APhantomOfTruth

grandGuignol :: CardDef
grandGuignol = (location
                 "03211"
                 ("Grand Guignol" <:> "Theatre of the Great Puppet")
                 [Paris]
                 Triangle
                 [Diamond, Square]
                 APhantomOfTruth
               )
  { cdVictoryPoints = Just 1
  }

operaGarnier212 :: CardDef
operaGarnier212 =
  location "03212" "Opéra Garnier" [Paris, Rail] Diamond [Triangle, Square, Heart] APhantomOfTruth

operaGarnier213 :: CardDef
operaGarnier213 =
  location "03213" "Opéra Garnier" [Paris, Rail] Diamond [Triangle, Square, Heart] APhantomOfTruth

gareDOrsay :: CardDef
gareDOrsay = location "03214" "Gare d'Orsay" [Paris, Rail] Heart [Diamond, Circle, Star] APhantomOfTruth

pereLachaiseCemetery :: CardDef
pereLachaiseCemetery =
  (location "03215" "Père Lachaise Cemetery" [Paris] T [Equals, Moon] APhantomOfTruth)
    { cdVictoryPoints = Just 1
    }

canalSaintMartin :: CardDef
canalSaintMartin =
  (location "03216" "Canal Saint-Martin" [Paris] Equals [Square, T, Moon] APhantomOfTruth)
    { cdVictoryPoints = Just 1
    }

leMarais217 :: CardDef
leMarais217 = location "03217" "Le Marais" [Paris, Rail] Moon [Square, Equals, T, Plus] APhantomOfTruth

leMarais218 :: CardDef
leMarais218 = location "03218" "Le Marais" [Paris, Rail] Moon [Square, Equals, T, Plus] APhantomOfTruth

notreDame :: CardDef
notreDame = location "03219" "Notre-Dame" [Paris, Rail] Plus [Circle, Moon, Star] APhantomOfTruth

gardensOfLuxembourg :: CardDef
gardensOfLuxembourg =
  (location "03220" "Gardens of Luxembourg" [Paris] Star [Circle, Heart, Plus] APhantomOfTruth)
    { cdVictoryPoints = Just 1
    }

theGateToHell :: CardDef
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

stoneArchways :: CardDef
stoneArchways = (locationWithUnrevealed
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
                )
  { cdEncounterSetQuantity = Just 2
  }

cryptOfTheSepulchralLamp :: CardDef
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

boneFilledCaverns :: CardDef
boneFilledCaverns = (locationWithUnrevealed
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
                    )
  { cdVictoryPoints = Just 1
  }

wellOfSouls :: CardDef
wellOfSouls =
  (locationWithUnrevealed
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
    )
    { cdVictoryPoints = Just 1
    }

candlelitTunnels :: CardDef
candlelitTunnels = (locationWithUnrevealed
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
                   )
  { cdEncounterSetQuantity = Just 2
  }

labyrinthOfBones :: CardDef
labyrinthOfBones = (locationWithUnrevealed
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
                   )
  { cdEncounterSetQuantity = Just 2
  }

narrowShaft :: CardDef
narrowShaft =
  (locationWithUnrevealed "03254" "Catacombs" [] NoSymbol [] "Narrow Shaft" [] NoSymbol [] ThePallidMask
    )
    { cdVictoryPoints = Just 1
    }

shiveringPools :: CardDef
shiveringPools = (locationWithUnrevealed
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
                 )
  { cdVictoryPoints = Just 1
  }

blockedPassage :: CardDef
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

tombOfShadows :: CardDef
tombOfShadows = (locationWithUnrevealed
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
                )
  { cdVictoryPoints = Just 1
  }

porteDeLAvancee :: CardDef
porteDeLAvancee = location "03283" "Porte de l’Avancée" [] BlackStarsRise

grandRue :: CardDef
grandRue = location "03284" "Grand Rue" [] BlackStarsRise

outerWall_285 :: CardDef
outerWall_285 =
  (location "03285" "Outer Wall" [] BlackStarsRise) { cdVictoryPoints = Just 1 }

outerWall_286 :: CardDef
outerWall_286 =
  (location "03286" "Outer Wall" [] BlackStarsRise) { cdVictoryPoints = Just 1 }

northTower_287 :: CardDef
northTower_287 = (location "03287" "North Tower" [] BlackStarsRise)
  { cdVictoryPoints = Just 1
  }

northTower_288 :: CardDef
northTower_288 = (location "03288" "North Tower" [] BlackStarsRise)
  { cdVictoryPoints = Just 1
  }

brokenSteps_289 :: CardDef
brokenSteps_289 = location "03289" "Broken Steps" [] BlackStarsRise

brokenSteps_290 :: CardDef
brokenSteps_290 = location "03290" "Broken Steps" [] BlackStarsRise

abbeyChurch :: CardDef
abbeyChurch = location "03291" "Abbey Church" [] BlackStarsRise

choeurGothique_292 :: CardDef
choeurGothique_292 = location "03292" "Chœur Gothique" [] BlackStarsRise

choeurGothique_293 :: CardDef
choeurGothique_293 = location "03293" "Chœur Gothique" [] BlackStarsRise

cloister :: CardDef
cloister = location "03294" "Cloister" [] BlackStarsRise

knightsHall :: CardDef
knightsHall = location "03295" "Knight's Hall" [] BlackStarsRise

chapelOfStAubertThePathIsOpen :: CardDef
chapelOfStAubertThePathIsOpen = locationWithUnrevealed
  "03296"
  "Chapel of St. Aubert"
  []
  ("Chapel of St. Aubert" <:> "The Path is Open")
  []
  BlackStarsRise

chapelOfStAubertWatersForbidden :: CardDef
chapelOfStAubertWatersForbidden = (locationWithUnrevealed
                                    "03297"
                                    "Chapel of St. Aubert"
                                    []
                                    ("Chapel of St. Aubert"
                                    <:> "Waters Forbidden"
                                    )
                                    []
                                    BlackStarsRise
                                  )
  { cdVictoryPoints = Just 2
  }

abbeyTowerThePathIsOpen :: CardDef
abbeyTowerThePathIsOpen = locationWithUnrevealed
  "03298"
  "Abbey Tower"
  []
  ("Abbey Tower" <:> "The Path is Open")
  []
  BlackStarsRise

abbeyTowerSpiresForbidden :: CardDef
abbeyTowerSpiresForbidden = (locationWithUnrevealed
                              "03299"
                              "Abbey Tower"
                              []
                              ("Abbey Tower" <:> "Spires Forbidden")
                              []
                              BlackStarsRise
                            )
  { cdVictoryPoints = Just 2
  }

shoresOfHali :: CardDef
shoresOfHali = location "03325b" "Shores of Hali" [Otherworld] DimCarcosa

bleakPlainsStarsOfAldebaran :: CardDef
bleakPlainsStarsOfAldebaran =
  location "03326b" "Bleak Plains" [Otherworld] DimCarcosa

bleakPlainsBleakDesolation :: CardDef
bleakPlainsBleakDesolation =
  location "03326d" "Bleak Plains" [Otherworld] DimCarcosa

ruinsOfCarcosaInhabitantOfCarcosa :: CardDef
ruinsOfCarcosaInhabitantOfCarcosa =
  location "03327b" "Ruins of Carcosa" [Otherworld] DimCarcosa

ruinsOfCarcosaAMomentsRest :: CardDef
ruinsOfCarcosaAMomentsRest =
  location "03327d" "Ruins of Carcosa" [Otherworld] DimCarcosa


ruinsOfCarcosaTheCoffin :: CardDef
ruinsOfCarcosaTheCoffin =
  location "03327f" "Ruins of Carcosa" [Otherworld] DimCarcosa

dimStreetsMappingTheStreets :: CardDef
dimStreetsMappingTheStreets =
  location "03328b" "Dim Streets" [Otherworld] DimCarcosa

dimStreetsTheKingsParade :: CardDef
dimStreetsTheKingsParade =
  location "03328d" "Dim Streets" [Otherworld] DimCarcosa

dimStreetsTheArchway :: CardDef
dimStreetsTheArchway = location "03328f" "Dim Streets" [Otherworld] DimCarcosa

depthsOfDemheTheHeightOfTheDepths :: CardDef
depthsOfDemheTheHeightOfTheDepths =
  location "03329b" "Depths of Demhe" [Otherworld] DimCarcosa

depthsOfDemheStepsOfThePalace :: CardDef
depthsOfDemheStepsOfThePalace =
  location "03329d" "Depths of Demhe" [Otherworld] DimCarcosa

darkSpires :: CardDef
darkSpires = location "03330b" "Dark Spires" [Otherworld] DimCarcosa

palaceOfTheKing :: CardDef
palaceOfTheKing =
  location "03331b" "Palace of the King" [Otherworld] DimCarcosa

expeditionCamp :: CardDef
expeditionCamp =
  location "04050" "Expedition Camp" [Campsite, Jungle] TheUntamedWilds

ruinsOfEztli :: CardDef
ruinsOfEztli =
  location "04053" "Ruins of Eztli" [Ancient, Ruins] TheUntamedWilds

pathOfThorns :: CardDef
pathOfThorns = location "04069" "Path of Thorns" [Jungle] Rainforest

riverCanyon :: CardDef
riverCanyon = location "04070" "River Canyon" [Jungle] Rainforest

ropeBridge :: CardDef
ropeBridge = location "04071" "Rope Bridge" [Jungle] Rainforest

serpentsHaven :: CardDef
serpentsHaven = location "04072" "Serpent's Haven" [Jungle] Rainforest

circuitousTrail :: CardDef
circuitousTrail = location "04073" "Circuitous Trail" [Jungle] Rainforest

templeOfTheFang :: CardDef
templeOfTheFang =
  location "04074" "Temple of the Fang" [Ancient, Ruins] Rainforest

overgrownRuins :: CardDef
overgrownRuins = location "04075" "Overgrown Ruins" [Ancient, Ruins] Rainforest

studyAberrantGateway :: CardDef
studyAberrantGateway =
  location "50013" ("Study" <:> "Aberrant Gateway") mempty ReturnToTheGathering

guestHall :: CardDef
guestHall = location "50014" "Guest Hall" mempty ReturnToTheGathering

bedroom :: CardDef
bedroom = location "50015" "Bedroom" mempty ReturnToTheGathering

bathroom :: CardDef
bathroom = location "50016" "Bathroom" mempty ReturnToTheGathering

holeInTheWall :: CardDef
holeInTheWall = locationWithUnrevealed
  "50017"
  "Hole in the Wall"
  mempty
  "Hallway"
  mempty
  ReturnToTheGathering

returnToAttic :: CardDef
returnToAttic = location "50018" "Attic" mempty ReturnToTheGathering

farAboveYourHouse :: CardDef
farAboveYourHouse = (locationWithUnrevealed
                      "50019"
                      "Far Above Your House"
                      mempty
                      "Field of Graves"
                      mempty
                      ReturnToTheGathering
                    )
  { cdVictoryPoints = Just 1
  }

returnToCellar :: CardDef
returnToCellar = location "50020" "Cellar" mempty ReturnToTheGathering

deepBelowYourHouse :: CardDef
deepBelowYourHouse = (locationWithUnrevealed
                       "50021"
                       "Deep Below Your House"
                       mempty
                       "Ghoul Pits"
                       mempty
                       ReturnToTheGathering
                     )
  { cdVictoryPoints = Just 1
  }

easttownArkhamPoliceStation :: CardDef
easttownArkhamPoliceStation = (location
                                "50027"
                                ("Easttown" <:> "Arkham Police Station")
                                [Arkham]
                                ReturnToTheMidnightMasks
                              )
  { cdVictoryPoints = Just 1
  }

northsideTrainStation :: CardDef
northsideTrainStation = location
  "50028"
  ("Northside" <:> "Train Station")
  [Arkham]
  ReturnToTheMidnightMasks

miskatonicUniversityMiskatonicMuseum :: CardDef
miskatonicUniversityMiskatonicMuseum = location
  "50029"
  ("Miskatonic University" <:> "Miskatonic Museum")
  [Arkham]
  ReturnToTheMidnightMasks

rivertownAbandonedWarehouse :: CardDef
rivertownAbandonedWarehouse = location
  "50030"
  ("Rivertown" <:> "Abandoned Warehouse")
  [Arkham, Central]
  ReturnToTheMidnightMasks

arkhamWoodsGreatWillow :: CardDef
arkhamWoodsGreatWillow = location
  "50033"
  ("Arkham Woods" <:> "Great Willow")
  [Woods]
  ReturnToTheDevourerBelow

arkhamWoodsLakeside :: CardDef
arkhamWoodsLakeside = location
  "50034"
  ("Arkham Woods" <:> "Lakeside")
  [Woods]
  ReturnToTheDevourerBelow

arkhamWoodsCorpseRiddenClearing :: CardDef
arkhamWoodsCorpseRiddenClearing = location
  "50035"
  ("Arkham Woods" <:> "Corpse-Ridden Clearing")
  [Woods]
  ReturnToTheDevourerBelow

arkhamWoodsWoodenBridge :: CardDef
arkhamWoodsWoodenBridge = location
  "50036"
  ("Arkham Woods" <:> "Wooden Bridge")
  [Woods]
  ReturnToTheDevourerBelow

cursedShores :: CardDef
cursedShores = location "81007" "Cursed Shores" [NewOrleans, Bayou] TheBayou

gardenDistrict :: CardDef
gardenDistrict = locationWithUnrevealed
  "81008"
  "New Orleans"
  [NewOrleans]
  "Garden District"
  [NewOrleans]
  TheBayou

broadmoor :: CardDef
broadmoor = (locationWithUnrevealed
              "81009"
              "New Orleans"
              [NewOrleans]
              "Broadmoor"
              [NewOrleans]
              TheBayou
            )
  { cdVictoryPoints = Just 1
  }

brackishWaters :: CardDef
brackishWaters = location "81010" "Brackish Waters" [Riverside, Bayou] TheBayou

audubonPark :: CardDef
audubonPark = (locationWithUnrevealed
                "81011"
                "Riverside"
                [Riverside]
                "Audubon Park"
                [Riverside]
                TheBayou
              )
  { cdVictoryPoints = Just 1
  }

fauborgMarigny :: CardDef
fauborgMarigny = locationWithUnrevealed
  "81012"
  "Riverside"
  [Riverside]
  "Fauborg Marigny"
  [Riverside]
  TheBayou

forgottenMarsh :: CardDef
forgottenMarsh =
  location "81013" "Forgotten Marsh" [Wilderness, Bayou] TheBayou

trappersCabin :: CardDef
trappersCabin = locationWithUnrevealed
  "81014"
  "Wilderness"
  [Wilderness]
  "Trapper's Cabin"
  [Wilderness]
  TheBayou

twistedUnderbrush :: CardDef
twistedUnderbrush = (locationWithUnrevealed
                      "81015"
                      "Wilderness"
                      [Wilderness]
                      "Twisted Underbrush"
                      [Wilderness]
                      TheBayou
                    )
  { cdVictoryPoints = Just 1
  }

foulSwamp :: CardDef
foulSwamp = location "81016" "Foul Swamp" [Unhallowed, Bayou] TheBayou

ritualGrounds :: CardDef
ritualGrounds = (locationWithUnrevealed
                  "81017"
                  "Unhallowed Land"
                  [Unhallowed]
                  "Ritual Grounds"
                  [Unhallowed]
                  TheBayou
                )
  { cdVictoryPoints = Just 1
  }

overgrownCairns :: CardDef
overgrownCairns = locationWithUnrevealed
  "81018"
  "Unhallowed Land"
  [Unhallowed]
  "Overgrown Cairns"
  [Unhallowed]
  TheBayou

gondola :: CardDef
gondola = location "82006b" "Gondola" [Venice, Boat] CarnevaleOfHorrors

sanMarcoBasilica :: CardDef
sanMarcoBasilica =
  location "82008" "San Marco Basilica" [Venice] CarnevaleOfHorrors

canalSide :: CardDef
canalSide = location "82009" "Canal-side" [Venice] CarnevaleOfHorrors

streetsOfVenice :: CardDef
streetsOfVenice =
  location "82010" "Streets of Venice" [Venice] CarnevaleOfHorrors

rialtoBridge :: CardDef
rialtoBridge =
  location "82011" "Rialto Bridge" [Venice, Bridge] CarnevaleOfHorrors

venetianGarden :: CardDef
venetianGarden = location "82012" "Venetian Garden" [Venice] CarnevaleOfHorrors

bridgeOfSighs :: CardDef
bridgeOfSighs =
  location "82013" "Bridge of Sighs" [Venice, Bridge] CarnevaleOfHorrors

floodedSquare :: CardDef
floodedSquare = location "82014" "Flooded Square" [Venice] CarnevaleOfHorrors

accademiaBridge :: CardDef
accademiaBridge =
  location "82015" "Accademia Bridge" [Venice, Bridge] CarnevaleOfHorrors

theGuardian :: CardDef
theGuardian = location "82016" "The Guardian" [Venice] CarnevaleOfHorrors
