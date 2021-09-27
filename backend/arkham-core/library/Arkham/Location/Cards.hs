module Arkham.Location.Cards where

import Arkham.Prelude

import Arkham.Types.Asset.Uses
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.CardType
import Arkham.Types.EncounterSet hiding (Dunwich)
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Name
import Arkham.Types.Trait

locationWithUnrevealed
  :: CardCode -> Name -> [Trait] -> Name -> [Trait] -> EncounterSet -> CardDef
locationWithUnrevealed cardCode unrevealedName unrevealedTraits name traits encounterSet
  = (location cardCode unrevealedName unrevealedTraits encounterSet)
    { cdRevealedName = Just name
    , cdRevealedCardTraits = setFromList traits
    }

location :: CardCode -> Name -> [Trait] -> EncounterSet -> CardDef
location cardCode name traits encounterSet = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Just name
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = LocationType
  , cdCardSubType = Nothing
  , cdClassSymbol = Nothing
  , cdSkills = mempty
  , cdCardTraits = setFromList traits
  , cdRevealedCardTraits = setFromList traits
  , cdKeywords = mempty
  , cdFastWindow = Nothing
  , cdAction = Nothing
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdCriteria = mempty
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
  }

allLocationCards :: HashMap CardCode CardDef
allLocationCards = mapFromList $ map
  (toCardCode &&& id)
  [ aTearInThePath
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
  , boxOffice
  , brackishWaters
  , bridgeOfSighs
  , broadmoor
  , burnedRuins_204
  , burnedRuins_205
  , canalSide
  , cellar
  , cloverClubBar
  , cloverClubCardroom
  , cloverClubLounge
  , coldSpringGlen_244
  , coldSpringGlen_245
  , congregationalChurch_208
  , congregationalChurch_209
  , courtyard
  , cursedShores
  , darkenedHall
  , deepBelowYourHouse
  , destroyedPath
  , devilsHopYard_252
  , devilsHopYard_253
  , dimensionalDoorway
  , dimensionalGap
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
  , gondola
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
  , laBellaLuna
  , lightingBox
  , livingRoom
  , lobby
  , lostMemories
  , mainPath
  , messHall
  , miskatonicQuad
  , miskatonicUniversity
  , miskatonicUniversityMiskatonicMuseum
  , museumEntrance
  , museumHalls
  , northside
  , northsideTrainStation
  , orneLibrary
  , osbornsGeneralStore_206
  , osbornsGeneralStore_207
  , overgrownCairns
  , parlor
  , parlorCar
  , passengerCar_167
  , passengerCar_168
  , passengerCar_169
  , passengerCar_170
  , passengerCar_171
  , patientConfinementDanielsCell
  , patientConfinementOccupiedCell
  , patientConfinementDrearyCell
  , patientConfinementFamiliarCell
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
  , sanMarcoBasilica
  , schoolhouse_212
  , schoolhouse_213
  , scienceBuilding
  , securityOffice_128
  , securityOffice_129
  , sentinelPeak
  , slaughteredWoods
  , sleepingCar
  , southsideHistoricalSociety
  , southsideMasBoardingHouse
  , stMarysHospital
  , stepsOfYhagharl
  , streetsOfVenice
  , studentUnion
  , study
  , studyAberrantGateway
  , tearThroughSpace
  , tearThroughTime
  , tenAcreMeadow_246
  , tenAcreMeadow_247
  , theEdgeOfTheUniverse
  , theGuardian
  , theHiddenChamber
  , theatre
  , trapRoom
  , trappersCabin
  , twistedUnderbrush
  , uprootedWoods
  , venetianGarden
  , villageCommons
  , vipArea
  , whateleyRuins_250
  , whateleyRuins_251
  , yard
  , yourHouse
  ]

study :: CardDef
study = location "01111" "Study" mempty TheGathering

hallway :: CardDef
hallway = location "01112" "Hallway" mempty TheGathering

attic :: CardDef
attic =
  (location "01113" "Attic" mempty TheGathering) { cdVictoryPoints = Just 1 }

cellar :: CardDef
cellar =
  (location "01114" "Cellar" mempty TheGathering) { cdVictoryPoints = Just 1 }

parlor :: CardDef
parlor = location "01115" "Parlor" mempty TheGathering

yourHouse :: CardDef
yourHouse = location "01124" "Your House" [Arkham] TheMidnightMasks

rivertown :: CardDef
rivertown = location "01125" "Rivertown" [Arkham, Central] TheMidnightMasks

southsideHistoricalSociety :: CardDef
southsideHistoricalSociety = location
  "01126"
  ("Southside" <:> "Historical Society")
  [Arkham]
  TheMidnightMasks

southsideMasBoardingHouse :: CardDef
southsideMasBoardingHouse = location
  "01127"
  ("Southside" <:> "Ma's Boarding House")
  [Arkham]
  TheMidnightMasks

stMarysHospital :: CardDef
stMarysHospital =
  location "01128" "St. Mary's Hospital" [Arkham] TheMidnightMasks

miskatonicUniversity :: CardDef
miskatonicUniversity =
  (location "01129" "Miskatonic University" [Arkham] TheMidnightMasks)
    { cdVictoryPoints = Just 1
    }

downtownFirstBankOfArkham :: CardDef
downtownFirstBankOfArkham = location
  "01130"
  ("Downtown" <:> "First Bank of Arkham")
  [Arkham]
  TheMidnightMasks

downtownArkhamAsylum :: CardDef
downtownArkhamAsylum =
  (location "01131" ("Downtown" <:> "Arkham Asylum") [Arkham] TheMidnightMasks)
    { cdVictoryPoints = Just 1
    }

easttown :: CardDef
easttown = location "01132" "Easttown" [Arkham] TheMidnightMasks

graveyard :: CardDef
graveyard = (location "01133" "Graveyard" [Arkham] TheMidnightMasks)
  { cdVictoryPoints = Just 1
  }

northside :: CardDef
northside = (location "01134" "Northside" [Arkham] TheMidnightMasks)
  { cdVictoryPoints = Just 1
  }

mainPath :: CardDef
mainPath = location "01149" "Main Path" [Woods] TheDevourerBelow

arkhamWoodsUnhallowedGround :: CardDef
arkhamWoodsUnhallowedGround = location
  "01150"
  ("Arkham Woods" <:> "Unhallowed Ground")
  [Woods]
  TheDevourerBelow

arkhamWoodsTwistingPaths :: CardDef
arkhamWoodsTwistingPaths = location
  "01151"
  ("Arkham Woods" <:> "Twisting Paths")
  [Woods]
  TheDevourerBelow

arkhamWoodsOldHouse :: CardDef
arkhamWoodsOldHouse =
  location "01152" ("Arkham Woods" <:> "Old House") [Woods] TheDevourerBelow

arkhamWoodsCliffside :: CardDef
arkhamWoodsCliffside =
  location "01153" ("Arkham Woods" <:> "Cliffside") [Woods] TheDevourerBelow

arkhamWoodsTangledThicket :: CardDef
arkhamWoodsTangledThicket = location
  "01154"
  ("Arkham Woods" <:> "Tangled Thicket")
  [Woods]
  TheDevourerBelow

arkhamWoodsQuietGlade :: CardDef
arkhamWoodsQuietGlade =
  location "01155" ("Arkham Woods" <:> "Quiet Glade") [Woods] TheDevourerBelow

ritualSite :: CardDef
ritualSite = location "01156" "Ritual Site" [Cave] TheDevourerBelow

miskatonicQuad :: CardDef
miskatonicQuad =
  location "02048" "Miskatonic Quad" [Miskatonic] ExtracurricularActivity

humanitiesBuilding :: CardDef
humanitiesBuilding =
  location "02049" "Humanities Building" [Miskatonic] ExtracurricularActivity

orneLibrary :: CardDef
orneLibrary =
  (location "02050" "Orne Library" [Miskatonic] ExtracurricularActivity)
    { cdVictoryPoints = Just 1
    }

studentUnion :: CardDef
studentUnion =
  location "02051" "Student Union" [Miskatonic] ExtracurricularActivity

dormitories :: CardDef
dormitories =
  (location "02052" "Dormitories" [Miskatonic] ExtracurricularActivity)
    { cdVictoryPoints = Just 1
    }

administrationBuilding :: CardDef
administrationBuilding = location
  "02053"
  "Administration Building"
  [Miskatonic]
  ExtracurricularActivity

facultyOfficesTheNightIsStillYoung :: CardDef
facultyOfficesTheNightIsStillYoung = (location
                                       "02054"
                                       ("Faculty Offices"
                                       <:> "The Night is Still Young"
                                       )
                                       [Miskatonic]
                                       ExtracurricularActivity
                                     )
  { cdVictoryPoints = Just 1
  }

facultyOfficesTheHourIsLate :: CardDef
facultyOfficesTheHourIsLate = location
  "02055"
  ("Faculty Offices" <:> "The Hour is Late")
  [Miskatonic]
  ExtracurricularActivity

scienceBuilding :: CardDef
scienceBuilding =
  location "02056" "Science Building" [Miskatonic] ExtracurricularActivity

alchemyLabs :: CardDef
alchemyLabs =
  location "02057" "Alchemy Labs" [Miskatonic] ExtracurricularActivity

laBellaLuna :: CardDef
laBellaLuna = location "02070" "La Bella Luna" [Arkham] TheHouseAlwaysWins

cloverClubLounge :: CardDef
cloverClubLounge =
  location "02071" "Clover Club Lounge" [CloverClub] TheHouseAlwaysWins

cloverClubBar :: CardDef
cloverClubBar =
  location "02072" "Clover Club Bar" [CloverClub] TheHouseAlwaysWins

cloverClubCardroom :: CardDef
cloverClubCardroom =
  location "02073" "Clover Club Cardroom" [CloverClub] TheHouseAlwaysWins

darkenedHall :: CardDef
darkenedHall = location "02074" "Darkened Hall" [CloverClub] TheHouseAlwaysWins

artGallery :: CardDef
artGallery = (locationWithUnrevealed
               "02075"
               "Back Hall Doorway"
               [CloverClub]
               "Art Gallery"
               [CloverClub]
               TheHouseAlwaysWins
             )
  { cdVictoryPoints = Just 1
  }

vipArea :: CardDef
vipArea = (locationWithUnrevealed
            "02076"
            "Back Hall Doorway"
            [CloverClub]
            "VIP Area"
            [CloverClub]
            TheHouseAlwaysWins
          )
  { cdVictoryPoints = Just 1
  }

backAlley :: CardDef
backAlley = (locationWithUnrevealed
              "02077"
              "Back Hall Doorway"
              [CloverClub]
              "Back Alley"
              [CloverClub]
              TheHouseAlwaysWins
            )
  { cdVictoryPoints = Just 1
  }

museumEntrance :: CardDef
museumEntrance =
  location "02126" "Museum Entrance" [Miskatonic] TheMiskatonicMuseum

museumHalls :: CardDef
museumHalls = location "02127" "Museum Halls" [Miskatonic] TheMiskatonicMuseum

securityOffice_128 :: CardDef
securityOffice_128 =
  location "02128" "Security Office" [Miskatonic] TheMiskatonicMuseum

securityOffice_129 :: CardDef
securityOffice_129 =
  location "02129" "Security Office" [Miskatonic] TheMiskatonicMuseum

administrationOffice_130 :: CardDef
administrationOffice_130 =
  location "02130" "Administration Office" [Miskatonic] TheMiskatonicMuseum

administrationOffice_131 :: CardDef
administrationOffice_131 =
  location "02131" "Administration Office" [Miskatonic] TheMiskatonicMuseum

exhibitHallAthabaskanExhibit :: CardDef
exhibitHallAthabaskanExhibit = locationWithUnrevealed
  "02132"
  "Exhibit Hall"
  [Miskatonic, Exhibit]
  ("Exhibit Hall" <:> "Athabaskan Exhibit")
  [Miskatonic, Exhibit]
  TheMiskatonicMuseum

exhibitHallMedusaExhibit :: CardDef
exhibitHallMedusaExhibit = (locationWithUnrevealed
                             "02133"
                             "Exhibit Hall"
                             [Miskatonic, Exhibit]
                             ("Exhibit Hall" <:> "Medusa Exhibit")
                             [Miskatonic, Exhibit]
                             TheMiskatonicMuseum
                           )
  { cdVictoryPoints = Just 1
  }

exhibitHallNatureExhibit :: CardDef
exhibitHallNatureExhibit = (locationWithUnrevealed
                             "02134"
                             "Exhibit Hall"
                             [Miskatonic, Exhibit]
                             ("Exhibit Hall" <:> "Nature Exhibit")
                             [Miskatonic, Exhibit]
                             TheMiskatonicMuseum
                           )
  { cdVictoryPoints = Just 1
  }

exhibitHallEgyptianExhibit :: CardDef
exhibitHallEgyptianExhibit = (locationWithUnrevealed
                               "02135"
                               "Exhibit Hall"
                               [Miskatonic, Exhibit]
                               ("Exhibit Hall" <:> "Egyptian Exhibit")
                               [Miskatonic, Exhibit]
                               TheMiskatonicMuseum
                             )
  { cdVictoryPoints = Just 1
  }

exhibitHallHallOfTheDead :: CardDef
exhibitHallHallOfTheDead = (locationWithUnrevealed
                             "02136"
                             "Exhibit Hall"
                             [Miskatonic, Exhibit]
                             ("Exhibit Hall" <:> "Hall of the Dead")
                             [Miskatonic, Exhibit]
                             TheMiskatonicMuseum
                           )
  { cdVictoryPoints = Just 1
  }

exhibitHallRestrictedHall :: CardDef
exhibitHallRestrictedHall = (locationWithUnrevealed
                              "02137"
                              "Exhibit Hall"
                              [Miskatonic, Exhibit]
                              ("Exhibit Hall" <:> "Restricted Hall")
                              [Miskatonic, Exhibit]
                              TheMiskatonicMuseum
                            )
  { cdVictoryPoints = Just 1
  }

passengerCar_167 :: CardDef
passengerCar_167 = locationWithUnrevealed
  "02167"
  "Train Car"
  [Train]
  "Passenger Car"
  [Train]
  TheEssexCountyExpress

passengerCar_168 :: CardDef
passengerCar_168 = locationWithUnrevealed
  "02168"
  "Train Car"
  [Train]
  "Passenger Car"
  [Train]
  TheEssexCountyExpress

passengerCar_169 :: CardDef
passengerCar_169 = locationWithUnrevealed
  "02169"
  "Train Car"
  [Train]
  "Passenger Car"
  [Train]
  TheEssexCountyExpress

passengerCar_170 :: CardDef
passengerCar_170 = locationWithUnrevealed
  "02170"
  "Train Car"
  [Train]
  "Passenger Car"
  [Train]
  TheEssexCountyExpress

passengerCar_171 :: CardDef
passengerCar_171 = locationWithUnrevealed
  "02171"
  "Train Car"
  [Train]
  "Passenger Car"
  [Train]
  TheEssexCountyExpress

sleepingCar :: CardDef
sleepingCar = locationWithUnrevealed
  "02172"
  "Train Car"
  [Train]
  "Sleeping Car"
  [Train]
  TheEssexCountyExpress

diningCar :: CardDef
diningCar = locationWithUnrevealed
  "02173"
  "Train Car"
  [Train]
  "Dining Car"
  [Train]
  TheEssexCountyExpress

parlorCar :: CardDef
parlorCar = (locationWithUnrevealed
              "02174"
              "Train Car"
              [Train]
              "Parlor Car"
              [Train]
              TheEssexCountyExpress
            )
  { cdVictoryPoints = Just 1
  }

engineCar_175 :: CardDef
engineCar_175 = (location "02175" "Engine Car" [Train] TheEssexCountyExpress)
  { cdVictoryPoints = Just 1
  }

engineCar_176 :: CardDef
engineCar_176 = (location "02176" "Engine Car" [Train] TheEssexCountyExpress)
  { cdVictoryPoints = Just 1
  }

engineCar_177 :: CardDef
engineCar_177 = (location "02177" "Engine Car" [Train] TheEssexCountyExpress)
  { cdVictoryPoints = Just 1
  }

villageCommons :: CardDef
villageCommons =
  location "02201" "Village Commons" [Dunwich, Central] BloodOnTheAltar

bishopsBrook_202 :: CardDef
bishopsBrook_202 = location "02202" "Bishop's Brook" [Dunwich] BloodOnTheAltar

bishopsBrook_203 :: CardDef
bishopsBrook_203 = location "02203" "Bishop's Brook" [Dunwich] BloodOnTheAltar

burnedRuins_204 :: CardDef
burnedRuins_204 = location "02204" "Burned Ruins" [Dunwich] BloodOnTheAltar

burnedRuins_205 :: CardDef
burnedRuins_205 = location "02205" "Burned Ruins" [Dunwich] BloodOnTheAltar

osbornsGeneralStore_206 :: CardDef
osbornsGeneralStore_206 =
  location "02206" "Osborn's General Store" [Dunwich] BloodOnTheAltar

osbornsGeneralStore_207 :: CardDef
osbornsGeneralStore_207 =
  location "02207" "Osborn's General Store" [Dunwich] BloodOnTheAltar

congregationalChurch_208 :: CardDef
congregationalChurch_208 =
  location "02208" "Congregational Church" [Dunwich] BloodOnTheAltar

congregationalChurch_209 :: CardDef
congregationalChurch_209 =
  location "02209" "Congregational Church" [Dunwich] BloodOnTheAltar

houseInTheReeds_210 :: CardDef
houseInTheReeds_210 =
  location "02210" "House in the Reeds" [Dunwich] BloodOnTheAltar

houseInTheReeds_211 :: CardDef
houseInTheReeds_211 =
  location "02211" "House in the Reeds" [Dunwich] BloodOnTheAltar

schoolhouse_212 :: CardDef
schoolhouse_212 = location "02212" "Schoolhouse" [Dunwich] BloodOnTheAltar

schoolhouse_213 :: CardDef
schoolhouse_213 = location "02213" "Schoolhouse" [Dunwich] BloodOnTheAltar

theHiddenChamber :: CardDef
theHiddenChamber = (location
                     "02214"
                     ("The Hidden Chamber" <:> "Prison of the Beast")
                     [Dunwich]
                     BloodOnTheAltar
                   )
  { cdVictoryPoints = Just 2
  }

dunwichVillage_242 :: CardDef
dunwichVillage_242 =
  location "02242" "Dunwich Village" [Dunwich] UndimensionedAndUnseen

dunwichVillage_243 :: CardDef
dunwichVillage_243 =
  location "02243" "Dunwich Village" [Dunwich] UndimensionedAndUnseen

coldSpringGlen_244 :: CardDef
coldSpringGlen_244 =
  location "02244" "Cold Spring Glen" [Dunwich] UndimensionedAndUnseen

coldSpringGlen_245 :: CardDef
coldSpringGlen_245 =
  location "02245" "Cold Spring Glen" [Dunwich] UndimensionedAndUnseen

tenAcreMeadow_246 :: CardDef
tenAcreMeadow_246 =
  location "02246" "Ten-Acre Meadow" [Dunwich] UndimensionedAndUnseen

tenAcreMeadow_247 :: CardDef
tenAcreMeadow_247 =
  location "02247" "Ten-Acre Meadow" [Dunwich] UndimensionedAndUnseen

blastedHeath_248 :: CardDef
blastedHeath_248 =
  location "02248" "Blasted Heath" [Dunwich] UndimensionedAndUnseen

blastedHeath_249 :: CardDef
blastedHeath_249 =
  location "02249" "Blasted Heath" [Dunwich] UndimensionedAndUnseen

whateleyRuins_250 :: CardDef
whateleyRuins_250 =
  location "02250" "Whateley Ruins" [Dunwich] UndimensionedAndUnseen

whateleyRuins_251 :: CardDef
whateleyRuins_251 =
  location "02251" "Whateley Ruins" [Dunwich] UndimensionedAndUnseen

devilsHopYard_252 :: CardDef
devilsHopYard_252 =
  location "02252" "Devil's Hop Yard" [Dunwich] UndimensionedAndUnseen

devilsHopYard_253 :: CardDef
devilsHopYard_253 =
  location "02253" "Devil's Hop Yard" [Dunwich] UndimensionedAndUnseen

baseOfTheHill :: CardDef
baseOfTheHill =
  location "02282" "Base of the Hill" [Dunwich, SentinelHill] WhereDoomAwaits

ascendingPath :: CardDef
ascendingPath =
  location "02283" "Ascending Path" [Dunwich, SentinelHill] WhereDoomAwaits

sentinelPeak :: CardDef
sentinelPeak =
  (location "02284" "Sentinel Peak" [Dunwich, SentinelHill] WhereDoomAwaits)
    { cdVictoryPoints = Just 2
    }

slaughteredWoods :: CardDef
slaughteredWoods = locationWithUnrevealed
  "02285"
  "Diverging Path"
  [Dunwich, Woods]
  "Slaughtered Woods"
  [Dunwich, Woods]
  WhereDoomAwaits

eerieGlade :: CardDef
eerieGlade = locationWithUnrevealed
  "02286"
  "Diverging Path"
  [Dunwich, Woods]
  "Eerie Glade"
  [Dunwich, Woods]
  WhereDoomAwaits

destroyedPath :: CardDef
destroyedPath = locationWithUnrevealed
  "02287"
  "Diverging Path"
  [Dunwich, Woods]
  "Destroyed Path"
  [Dunwich, Woods]
  WhereDoomAwaits

frozenSpring :: CardDef
frozenSpring = locationWithUnrevealed
  "02288"
  "Diverging Path"
  [Dunwich, Woods]
  "Frozen Spring"
  [Dunwich, Woods]
  WhereDoomAwaits

dimensionalGap :: CardDef
dimensionalGap = locationWithUnrevealed
  "02289"
  "Altered Path"
  [Dunwich, Woods, Altered]
  "Dimensional Gap"
  [Dunwich, Woods, Altered]
  WhereDoomAwaits

aTearInThePath :: CardDef
aTearInThePath = locationWithUnrevealed
  "02290"
  "Altered Path"
  [Dunwich, Woods, Altered]
  "A Tear in the Path"
  [Dunwich, Woods, Altered]
  WhereDoomAwaits

uprootedWoods :: CardDef
uprootedWoods = locationWithUnrevealed
  "02291"
  "Altered Path"
  [Dunwich, Woods, Altered]
  "Uprooted Woods"
  [Dunwich, Woods, Altered]
  WhereDoomAwaits

lostMemories :: CardDef
lostMemories = locationWithUnrevealed
  "02292"
  "Altered Path"
  [Dunwich, Woods, Altered]
  "Lost Memories"
  [Dunwich, Woods, Altered]
  WhereDoomAwaits

anotherDimension :: CardDef
anotherDimension = location
  "02320"
  ("Another Dimension" <:> "Unfettered by Reality")
  [Otherworld]
  LostInTimeAndSpace

theEdgeOfTheUniverse :: CardDef
theEdgeOfTheUniverse =
  location "02321" "The Edge of the Universe" [Otherworld] LostInTimeAndSpace

tearThroughTime :: CardDef
tearThroughTime =
  location "02322" "Tear Through Time" [Otherworld] LostInTimeAndSpace

tearThroughSpace :: CardDef
tearThroughSpace = (location
                     "02324"
                     "Tear Through Space"
                     [Otherworld, Extradimensional]
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
                    LostInTimeAndSpace
                  )
  { cdDoubleSided = False
  }

dimensionalDoorway :: CardDef
dimensionalDoorway = (location
                       "02328"
                       "Dimensional Doorway"
                       [Otherworld, Extradimensional]
                       LostInTimeAndSpace
                     )
  { cdDoubleSided = False
  }

theatre :: CardDef
theatre = location "03049" "Theatre" mempty CurtainCall

lobby :: CardDef
lobby = location "03050" "Lobby" mempty CurtainCall

balcony :: CardDef
balcony =
  (location "03051" "Balcony" mempty CurtainCall) { cdVictoryPoints = Just 1 }

backstage :: CardDef
backstage = location "03052" "Backstage" mempty CurtainCall

lightingBox :: CardDef
lightingBox = (locationWithUnrevealed
                "03053"
                "Lobby Doorway"
                [Private]
                "Lighting Box"
                [Private]
                CurtainCall
              )
  { cdVictoryPoints = Just 1
  }

boxOffice :: CardDef
boxOffice = locationWithUnrevealed
  "03054"
  "Lobby Doorway"
  [Private]
  "Box Office"
  [Private]
  CurtainCall

greenRoom :: CardDef
greenRoom = (locationWithUnrevealed
              "03055"
              "Lobby Doorway"
              [Private]
              "Green Room"
              [Private]
              CurtainCall
            )
  { cdVictoryPoints = Just 1
  }

dressingRoom :: CardDef
dressingRoom = locationWithUnrevealed
  "03056"
  "Backstage Doorway"
  [Private]
  "Dressing Room"
  [Private]
  CurtainCall

rehearsalRoom :: CardDef
rehearsalRoom = (locationWithUnrevealed
                  "03057"
                  "Backstage Doorway"
                  [Private]
                  "Rehearsal Room"
                  [Private]
                  CurtainCall
                )
  { cdVictoryPoints = Just 1
  }

trapRoom :: CardDef
trapRoom = (locationWithUnrevealed
             "03058"
             "Backstage Doorway"
             [Private]
             "Trap Room"
             [Private]
             CurtainCall
           )
  { cdVictoryPoints = Just 1
  }

foyer :: CardDef
foyer = location "03070" "Foyer" mempty TheLastKing

ballroom :: CardDef
ballroom = location "03071" "Ballroom" mempty TheLastKing

livingRoom :: CardDef
livingRoom = location "03072" "Living Room" mempty TheLastKing

gallery :: CardDef
gallery = location "03073" "Gallery" mempty TheLastKing

courtyard :: CardDef
courtyard = location "03074" "Courtyard" mempty TheLastKing

diningRoom :: CardDef
diningRoom = location "03075" "Dining Room" mempty TheLastKing

entryHall :: CardDef
entryHall = location "03127" "Entry Hall" [GroundFloor] EchoesOfThePast

historicalSocietyMeetingRoom :: CardDef
historicalSocietyMeetingRoom = locationWithUnrevealed
  "03128"
  "Historical Society"
  [GroundFloor]
  ("Historical Society" <:> "Meeting Room")
  [GroundFloor, Passageway]
  EchoesOfThePast

historicalSocietyRecordOffice_129 :: CardDef
historicalSocietyRecordOffice_129 = locationWithUnrevealed
  "03129"
  "Historical Society"
  [GroundFloor]
  ("Historical Society" <:> "Record Office")
  [GroundFloor]
  EchoesOfThePast

historicalSocietyHistoricalMuseum_130 :: CardDef
historicalSocietyHistoricalMuseum_130 = locationWithUnrevealed
  "03130"
  "Historical Society"
  [GroundFloor]
  ("Historical Society" <:> "Historical Museum")
  [GroundFloor]
  EchoesOfThePast

quietHalls_131 :: CardDef
quietHalls_131 = location "03131" "Quiet Halls" [SecondFloor] EchoesOfThePast

historicalSocietyHistoricalMuseum_132 :: CardDef
historicalSocietyHistoricalMuseum_132 = locationWithUnrevealed
  "03132"
  "Historical Society"
  [SecondFloor]
  ("Historical Society" <:> "Historical Museum")
  [SecondFloor]
  EchoesOfThePast

historicalSocietyHistoricalLibrary_133 :: CardDef
historicalSocietyHistoricalLibrary_133 = locationWithUnrevealed
  "03133"
  "Historical Society"
  [SecondFloor]
  ("Historical Society" <:> "Historical Library")
  [SecondFloor, Passageway]
  EchoesOfThePast

historicalSocietyReadingRoom :: CardDef
historicalSocietyReadingRoom = locationWithUnrevealed
  "03134"
  "Historical Society"
  [SecondFloor]
  ("Historical Society" <:> "Reading Room")
  [SecondFloor]
  EchoesOfThePast

quietHalls_135 :: CardDef
quietHalls_135 = location "03135" "Quiet Halls" [ThirdFloor] EchoesOfThePast

historicalSocietyHistoricalLibrary_136 :: CardDef
historicalSocietyHistoricalLibrary_136 = locationWithUnrevealed
  "03136"
  "Historical Society"
  [ThirdFloor]
  ("Historical Society" <:> "Historical Library")
  [ThirdFloor, Passageway]
  EchoesOfThePast

historicalSocietyPeabodysOffice :: CardDef
historicalSocietyPeabodysOffice = locationWithUnrevealed
  "03137"
  "Historical Society"
  [ThirdFloor]
  ("Historical Society" <:> "Peabody's Office")
  [ThirdFloor, Passageway]
  EchoesOfThePast

historicalSocietyRecordOffice_138 :: CardDef
historicalSocietyRecordOffice_138 = locationWithUnrevealed
  "03138"
  "Historical Society"
  [ThirdFloor]
  ("Historical Society" <:> "Record Office")
  [ThirdFloor]
  EchoesOfThePast

hiddenLibrary :: CardDef
hiddenLibrary = (location "03139" "Hidden Library" mempty EchoesOfThePast)
  { cdVictoryPoints = Just 2
  }

asylumHallsWesternPatientWing_168 :: CardDef
asylumHallsWesternPatientWing_168 = location
  "03168"
  ("Asylum Halls" <:> "Western Patient Wing")
  mempty
  TheUnspeakableOath

asylumHallsWesternPatientWing_169 :: CardDef
asylumHallsWesternPatientWing_169 = location
  "03169"
  ("Asylum Halls" <:> "Western Patient Wing")
  mempty
  TheUnspeakableOath

asylumHallsEasternPatientWing_170 :: CardDef
asylumHallsEasternPatientWing_170 = location
  "03170"
  ("Asylum Halls" <:> "Eastern Patient Wing")
  mempty
  TheUnspeakableOath

asylumHallsEasternPatientWing_171 :: CardDef
asylumHallsEasternPatientWing_171 = location
  "03171"
  ("Asylum Halls" <:> "Eastern Patient Wing")
  mempty
  TheUnspeakableOath

kitchen :: CardDef
kitchen = location "03172" "Kitchen" mempty TheUnspeakableOath

messHall :: CardDef
messHall = location "03173" "Mess Hall" mempty TheUnspeakableOath

infirmary :: CardDef
infirmary = location "03174" "Infirmary" mempty TheUnspeakableOath

yard :: CardDef
yard = location "03175" "Yard" mempty TheUnspeakableOath

garden :: CardDef
garden = location "03176" "Garden" mempty TheUnspeakableOath

basementHall :: CardDef
basementHall = location "03177" "Basement Hall" mempty TheUnspeakableOath

patientConfinementDanielsCell :: CardDef
patientConfinementDanielsCell = location
  "03178"
  ("Patient Confinement" <:> "Daniel's Cell")
  mempty
  TheUnspeakableOath

patientConfinementOccupiedCell :: CardDef
patientConfinementOccupiedCell = location
  "03179"
  ("Patient Confinement" <:> "Occupied Cell")
  mempty
  TheUnspeakableOath

patientConfinementDrearyCell :: CardDef
patientConfinementDrearyCell = location
  "03180"
  ("Patient Confinement" <:> "Dreary Cell")
  mempty
  TheUnspeakableOath

patientConfinementFamiliarCell :: CardDef
patientConfinementFamiliarCell = location
  "03181"
  ("Patient Confinement" <:> "Familiar Cell")
  mempty
  TheUnspeakableOath

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
