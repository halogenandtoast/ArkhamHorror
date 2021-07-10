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
  , cdFastWindow = Nothing
  , cdAction = Nothing
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdPlayRestrictions = mempty
  , cdCommitRestrictions = mempty
  , cdAttackOfOpportunityModifiers = mempty
  , cdPermanent = False
  , cdEncounterSet = Just encounterSet
  , cdEncounterSetQuantity = Just 1
  , cdUnique = False
  , cdDoubleSided = True
  , cdLimits = []
  }

allLocationCards :: HashMap CardCode CardDef
allLocationCards = mapFromList $ map
  (toCardCode &&& id)
  [ placeholderLocation
  , aTearInThePath
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
  , attic
  , audubonPark
  , backAlley
  , baseOfTheHill
  , bathroom
  , bedroom
  , bishopsBrook_202
  , bishopsBrook_203
  , blastedHeath_248
  , blastedHeath_249
  , brackishWaters
  , broadmoor
  , burnedRuins_204
  , burnedRuins_205
  , cellar
  , cloverClubBar
  , cloverClubCardroom
  , cloverClubLounge
  , coldSpringGlen_244
  , coldSpringGlen_245
  , congregationalChurch_208
  , congregationalChurch_209
  , cursedShores
  , darkenedHall
  , deepBelowYourHouse
  , destroyedPath
  , devilsHopYard_252
  , devilsHopYard_253
  , dimensionalDoorway
  , dimensionalGap
  , diningCar
  , dormitories
  , downtownArkhamAsylum
  , downtownFirstBankOfArkham
  , dunwichVillage_242
  , dunwichVillage_243
  , easttown
  , easttownArkhamPoliceStation
  , eerieGlade
  , endlessBridge
  , engineCar_175
  , engineCar_176
  , engineCar_177
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
  , forgottenMarsh
  , foulSwamp
  , frozenSpring
  , gardenDistrict
  , graveyard
  , guestHall
  , hallway
  , holeInTheWall
  , houseInTheReeds_210
  , houseInTheReeds_211
  , humanitiesBuilding
  , laBellaLuna
  , lostMemories
  , mainPath
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
  , prismaticCascade
  , returnToAttic
  , returnToCellar
  , ritualGrounds
  , ritualSite
  , rivertown
  , rivertownAbandonedWarehouse
  , schoolhouse_212
  , schoolhouse_213
  , scienceBuilding
  , securityOffice_128
  , securityOffice_129
  , sentinelPeak
  , slaugteredWoods
  , sleepingCar
  , southsideHistoricalSociety
  , southsideMasBoardingHouse
  , stMarysHospital
  , stepsOfYhagharl
  , studentUnion
  , study
  , studyAberrantGateway
  , tearThroughSpace
  , tearThroughTime
  , tenAcreMeadow_246
  , tenAcreMeadow_247
  , theEdgeOfTheUniverse
  , theHiddenChamber
  , trappersCabin
  , twistedUnderbrush
  , uprootedWoods
  , villageCommons
  , vipArea
  , whateleyRuins_250
  , whateleyRuins_251
  , yourHouse
  ]

placeholderLocation :: CardDef
placeholderLocation = location "location" "Placeholder Location Card" Test

study :: CardDef
study = location "01111" "Study" TheGathering

hallway :: CardDef
hallway = location "01112" "Hallway" TheGathering

attic :: CardDef
attic = (location "01113" "Attic" TheGathering) { cdVictoryPoints = Just 1 }

cellar :: CardDef
cellar = (location "01114" "Cellar" TheGathering) { cdVictoryPoints = Just 1 }

parlor :: CardDef
parlor = location "01115" "Parlor" TheGathering

yourHouse :: CardDef
yourHouse = (location "01124" "Your House" TheMidnightMasks)
  { cdCardTraits = singleton Arkham
  }

rivertown :: CardDef
rivertown = (location "01125" "Rivertown" TheMidnightMasks)
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
stMarysHospital = (location "01128" "St. Mary's Hospital" TheMidnightMasks)
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
easttown = (location "01132" "Easttown" TheMidnightMasks)
  { cdCardTraits = singleton Arkham
  }

graveyard :: CardDef
graveyard = (location "01133" "Graveyard" TheMidnightMasks)
  { cdCardTraits = singleton Arkham
  , cdVictoryPoints = Just 1
  }

northside :: CardDef
northside = (location "01134" "Northside" TheMidnightMasks)
  { cdCardTraits = singleton Arkham
  , cdVictoryPoints = Just 1
  }

mainPath :: CardDef
mainPath = (location "01149" "Main Path" TheDevourerBelow)
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
ritualSite = (location "01156" "Ritual Site" TheDevourerBelow)
  { cdCardTraits = singleton Cave
  }

miskatonicQuad :: CardDef
miskatonicQuad = (location "02048" "Miskatonic Quad" ExtracurricularActivity)
  { cdCardTraits = singleton Miskatonic
  }

humanitiesBuilding :: CardDef
humanitiesBuilding =
  (location "02049" "Humanities Building" ExtracurricularActivity)
    { cdCardTraits = singleton Miskatonic
    }

orneLibrary :: CardDef
orneLibrary = (location "02050" "Orne Library" ExtracurricularActivity)
  { cdCardTraits = singleton Miskatonic
  , cdVictoryPoints = Just 1
  }

studentUnion :: CardDef
studentUnion = (location "02051" "Student Union" ExtracurricularActivity)
  { cdCardTraits = singleton Miskatonic
  }

dormitories :: CardDef
dormitories = (location "02052" "Dormitories" ExtracurricularActivity)
  { cdCardTraits = singleton Miskatonic
  , cdVictoryPoints = Just 1
  }

administrationBuilding :: CardDef
administrationBuilding =
  (location "02053" "Administration Building" ExtracurricularActivity)
    { cdCardTraits = singleton Miskatonic
    }

facultyOfficesTheNightIsStillYoung :: CardDef
facultyOfficesTheNightIsStillYoung = (location
                                       "02054"
                                       ("Faculty Offices"
                                       <:> "The Night is Still Young"
                                       )
                                       ExtracurricularActivity
                                     )
  { cdCardTraits = singleton Miskatonic
  , cdVictoryPoints = Just 1
  }

facultyOfficesTheHourIsLate :: CardDef
facultyOfficesTheHourIsLate = (location
                                "02055"
                                ("Faculty Offices" <:> "The Hour is Late")
                                ExtracurricularActivity
                              )
  { cdCardTraits = singleton Miskatonic
  }

scienceBuilding :: CardDef
scienceBuilding = (location "02056" "Science Building" ExtracurricularActivity)
  { cdCardTraits = singleton Miskatonic
  }

alchemyLabs :: CardDef
alchemyLabs = (location "02057" "Alchemy Labs" ExtracurricularActivity)
  { cdCardTraits = singleton Miskatonic
  }

laBellaLuna :: CardDef
laBellaLuna = (location "02070" "La Bella Luna" TheHouseAlwaysWins)
  { cdCardTraits = singleton Arkham
  }

cloverClubLounge :: CardDef
cloverClubLounge = (location "02071" "Clover Club Lounge" TheHouseAlwaysWins)
  { cdCardTraits = singleton CloverClub
  }

cloverClubBar :: CardDef
cloverClubBar = (location "02072" "Clover Club Bar" TheHouseAlwaysWins)
  { cdCardTraits = singleton CloverClub
  }

cloverClubCardroom :: CardDef
cloverClubCardroom =
  (location "02073" "Clover Club Cardroom" TheHouseAlwaysWins)
    { cdCardTraits = singleton CloverClub
    }

darkenedHall :: CardDef
darkenedHall = (location "02074" "Darkened Hall" TheHouseAlwaysWins)
  { cdCardTraits = singleton CloverClub
  }

artGallery :: CardDef
artGallery = (location "02075" "Art Gallery" TheHouseAlwaysWins)
  { cdCardTraits = singleton CloverClub
  , cdVictoryPoints = Just 1
  }

vipArea :: CardDef
vipArea = (location "02076" "VIP Area" TheHouseAlwaysWins)
  { cdCardTraits = singleton CloverClub
  , cdVictoryPoints = Just 1
  }

backAlley :: CardDef
backAlley = (location "02077" "Back Alley" TheHouseAlwaysWins)
  { cdCardTraits = singleton CloverClub
  , cdVictoryPoints = Just 1
  }

museumEntrance :: CardDef
museumEntrance = (location "02126" "Museum Entrance" TheMiskatonicMuseum)
  { cdCardTraits = singleton Miskatonic
  }

museumHalls :: CardDef
museumHalls = (location "02127" "Museum Halls" TheMiskatonicMuseum)
  { cdCardTraits = singleton Miskatonic
  }

securityOffice_128 :: CardDef
securityOffice_128 = (location "02128" "Security Office" TheMiskatonicMuseum)
  { cdCardTraits = singleton Miskatonic
  }

securityOffice_129 :: CardDef
securityOffice_129 = (location "02129" "Security Office" TheMiskatonicMuseum)
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
  (location
      "02132"
      ("Exhibit Hall" <:> "Athabaskan Exhibit")
      TheMiskatonicMuseum
    )
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
passengerCar_167 = (location "02167" "Passenger Car" TheEssexCountyExpress)
  { cdCardTraits = singleton Train
  }

passengerCar_168 :: CardDef
passengerCar_168 = (location "02168" "Passenger Car" TheEssexCountyExpress)
  { cdCardTraits = singleton Train
  }

passengerCar_169 :: CardDef
passengerCar_169 = (location "02169" "Passenger Car" TheEssexCountyExpress)
  { cdCardTraits = singleton Train
  }

passengerCar_170 :: CardDef
passengerCar_170 = (location "02170" "Passenger Car" TheEssexCountyExpress)
  { cdCardTraits = singleton Train
  }

passengerCar_171 :: CardDef
passengerCar_171 = (location "02171" "Passenger Car" TheEssexCountyExpress)
  { cdCardTraits = singleton Train
  }

sleepingCar :: CardDef
sleepingCar = (location "02172" "Sleeping Car" TheEssexCountyExpress)
  { cdCardTraits = singleton Train
  }

diningCar :: CardDef
diningCar = (location "02173" "Dining Car" TheEssexCountyExpress)
  { cdCardTraits = singleton Train
  }

parlorCar :: CardDef
parlorCar = (location "02174" "Parlor Car" TheEssexCountyExpress)
  { cdCardTraits = singleton Train
  , cdVictoryPoints = Just 1
  }

engineCar_175 :: CardDef
engineCar_175 = (location "02175" "Engine Car" TheEssexCountyExpress)
  { cdCardTraits = singleton Train
  , cdVictoryPoints = Just 1
  }

engineCar_176 :: CardDef
engineCar_176 = (location "02176" "Engine Car" TheEssexCountyExpress)
  { cdCardTraits = singleton Train
  , cdVictoryPoints = Just 1
  }

engineCar_177 :: CardDef
engineCar_177 = (location "02177" "Engine Car" TheEssexCountyExpress)
  { cdCardTraits = singleton Train
  , cdVictoryPoints = Just 1
  }

villageCommons :: CardDef
villageCommons = (location "02201" "Village Commons" BloodOnTheAltar)
  { cdCardTraits = setFromList [Dunwich, Central]
  }

bishopsBrook_202 :: CardDef
bishopsBrook_202 = (location "02202" "Bishop's Brook" BloodOnTheAltar)
  { cdCardTraits = singleton Dunwich
  }

bishopsBrook_203 :: CardDef
bishopsBrook_203 = (location "02203" "Bishop's Brook" BloodOnTheAltar)
  { cdCardTraits = singleton Dunwich
  }

burnedRuins_204 :: CardDef
burnedRuins_204 = (location "02204" "Burned Ruins" BloodOnTheAltar)
  { cdCardTraits = singleton Dunwich
  }

burnedRuins_205 :: CardDef
burnedRuins_205 = (location "02205" "Burned Ruins" BloodOnTheAltar)
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
houseInTheReeds_210 = (location "02210" "House in the Reeds" BloodOnTheAltar)
  { cdCardTraits = singleton Dunwich
  }

houseInTheReeds_211 :: CardDef
houseInTheReeds_211 = (location "02211" "House in the Reeds" BloodOnTheAltar)
  { cdCardTraits = singleton Dunwich
  }

schoolhouse_212 :: CardDef
schoolhouse_212 = (location "02212" "Schoolhouse" BloodOnTheAltar)
  { cdCardTraits = singleton Dunwich
  }

schoolhouse_213 :: CardDef
schoolhouse_213 = (location "02213" "Schoolhouse" BloodOnTheAltar)
  { cdCardTraits = singleton Dunwich
  }

theHiddenChamber :: CardDef
theHiddenChamber = (location
                     "02214"
                     ("The Hidden Chamber" <:> "Prison of the Beast")
                     BloodOnTheAltar
                   )
  { cdCardTraits = singleton Dunwich
  , cdVictoryPoints = Just 2
  }

dunwichVillage_242 :: CardDef
dunwichVillage_242 = (location "02242" "Dunwich Village" UndimensionedAndUnseen
                     )
  { cdCardTraits = setFromList [Dunwich]
  }

dunwichVillage_243 :: CardDef
dunwichVillage_243 = (location "02243" "Dunwich Village" UndimensionedAndUnseen
                     )
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
tenAcreMeadow_246 = (location "02246" "Ten-Acre Meadow" UndimensionedAndUnseen)
  { cdCardTraits = setFromList [Dunwich]
  }

tenAcreMeadow_247 :: CardDef
tenAcreMeadow_247 = (location "02247" "Ten-Acre Meadow" UndimensionedAndUnseen)
  { cdCardTraits = setFromList [Dunwich]
  }

blastedHeath_248 :: CardDef
blastedHeath_248 = (location "02248" "Blasted Heath" UndimensionedAndUnseen)
  { cdCardTraits = setFromList [Dunwich]
  }

blastedHeath_249 :: CardDef
blastedHeath_249 = (location "02249" "Blasted Heath" UndimensionedAndUnseen)
  { cdCardTraits = setFromList [Dunwich]
  }

whateleyRuins_250 :: CardDef
whateleyRuins_250 = (location "02250" "Whateley Ruins" UndimensionedAndUnseen)
  { cdCardTraits = setFromList [Dunwich]
  }

whateleyRuins_251 :: CardDef
whateleyRuins_251 = (location "02251" "Whateley Ruins" UndimensionedAndUnseen)
  { cdCardTraits = setFromList [Dunwich]
  }

devilsHopYard_252 :: CardDef
devilsHopYard_252 = (location "02252" "Devil's Hop Yard" UndimensionedAndUnseen
                    )
  { cdCardTraits = setFromList [Dunwich]
  }

devilsHopYard_253 :: CardDef
devilsHopYard_253 = (location "02253" "Devil's Hop Yard" UndimensionedAndUnseen
                    )
  { cdCardTraits = setFromList [Dunwich]
  }

baseOfTheHill :: CardDef
baseOfTheHill = (location "02282" "Base of the Hill" WhereDoomAwaits)
  { cdCardTraits = setFromList [Dunwich, SentinelHill]
  }

ascendingPath :: CardDef
ascendingPath = (location "02283" "Ascending Path" WhereDoomAwaits)
  { cdCardTraits = setFromList [Dunwich, SentinelHill]
  }

sentinelPeak :: CardDef
sentinelPeak = (location "02284" "Sentinel Peak" WhereDoomAwaits)
  { cdCardTraits = setFromList [Dunwich, SentinelHill]
  , cdVictoryPoints = Just 2
  }

slaugteredWoods :: CardDef
slaugteredWoods = (location "02285" "Slaughtered Woods" WhereDoomAwaits)
  { cdCardTraits = setFromList [Dunwich, Woods]
  }

eerieGlade :: CardDef
eerieGlade = (location "02286" "Eerie Glade" WhereDoomAwaits)
  { cdCardTraits = setFromList [Dunwich, Woods]
  }

destroyedPath :: CardDef
destroyedPath = (location "02287" "Destroyed Path" WhereDoomAwaits)
  { cdCardTraits = setFromList [Dunwich, Woods]
  }

frozenSpring :: CardDef
frozenSpring = (location "02288" "Frozen Spring" WhereDoomAwaits)
  { cdCardTraits = setFromList [Dunwich, Woods]
  }

dimensionalGap :: CardDef
dimensionalGap = (location "02289" "Dimensional Gap" WhereDoomAwaits)
  { cdCardTraits = setFromList [Dunwich, Woods, Altered]
  }

aTearInThePath :: CardDef
aTearInThePath = (location "02290" "A Tear in the Path" WhereDoomAwaits)
  { cdCardTraits = setFromList [Dunwich, Woods, Altered]
  }

uprootedWoods :: CardDef
uprootedWoods = (location "02291" "Uprooted Woods" WhereDoomAwaits)
  { cdCardTraits = setFromList [Dunwich, Woods, Altered]
  }

lostMemories :: CardDef
lostMemories = (location "02292" "Lost Memories" WhereDoomAwaits)
  { cdCardTraits = setFromList [Dunwich, Woods, Altered]
  }

anotherDimension :: CardDef
anotherDimension = (location
                     "02320"
                     ("Another Dimension" <:> "Unfettered by Reality")
                     LostInTimeAndSpace
                   )
  { cdCardTraits = setFromList [Otherworld]
  }

theEdgeOfTheUniverse :: CardDef
theEdgeOfTheUniverse =
  (location "02321" "The Edge of the Universe" LostInTimeAndSpace)
    { cdCardTraits = setFromList [Otherworld]
    }

tearThroughTime :: CardDef
tearThroughTime = (location "02322" "Tear Through Time" LostInTimeAndSpace)
  { cdCardTraits = setFromList [Otherworld]
  }

tearThroughSpace :: CardDef
tearThroughSpace = (location "02324" "Tear Through Space" LostInTimeAndSpace)
  { cdCardTraits = setFromList [Otherworld, Extradimensional]
  , cdKeywords = setFromList [Keyword.Surge]
  , cdDoubleSided = False
  , cdEncounterSetQuantity = Just 4
  }

prismaticCascade :: CardDef
prismaticCascade = (location "02325" "Prismatic Cascade" LostInTimeAndSpace)
  { cdCardTraits = setFromList [Otherworld, Extradimensional]
  , cdDoubleSided = False
  , cdEncounterSetQuantity = Just 2
  }

endlessBridge :: CardDef
endlessBridge = (location "02326" "Endless Bridge" LostInTimeAndSpace)
  { cdCardTraits = setFromList [Otherworld, Extradimensional]
  , cdDoubleSided = False
  , cdEncounterSetQuantity = Just 2
  }

stepsOfYhagharl :: CardDef
stepsOfYhagharl = (location "02327" "Steps of Y'hagharl" LostInTimeAndSpace)
  { cdCardTraits = setFromList [Otherworld, Extradimensional]
  }

dimensionalDoorway :: CardDef
dimensionalDoorway = (location "02328" "Dimensional Doorway" LostInTimeAndSpace
                     )
  { cdCardTraits = setFromList [Otherworld, Extradimensional]
  }

studyAberrantGateway :: CardDef
studyAberrantGateway =
  location "50013" ("Study" <:> "Aberrant Gateway") ReturnToTheGathering

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
farAboveYourHouse = (location "50019" "Field of Graves" ReturnToTheGathering)
  { cdVictoryPoints = Just 1
  }

returnToCellar :: CardDef
returnToCellar = location "50020" "Cellar" ReturnToTheGathering

deepBelowYourHouse :: CardDef
deepBelowYourHouse = (location "50021" "Ghoul Pits" ReturnToTheGathering)
  { cdVictoryPoints = Just 1
  }

easttownArkhamPoliceStation :: CardDef
easttownArkhamPoliceStation = (location
                                "50027"
                                ("Easttown" <:> "Arkham Police Station")
                                ReturnToTheMidnightMasks
                              )
  { cdCardTraits = setFromList [Arkham]
  , cdVictoryPoints = Just 1
  }

northsideTrainStation :: CardDef
northsideTrainStation =
  (location "50028" ("Northside" <:> "Train Station") ReturnToTheMidnightMasks)
    { cdCardTraits = setFromList [Arkham]
    }

miskatonicUniversityMiskatonicMuseum :: CardDef
miskatonicUniversityMiskatonicMuseum = (location
                                         "50029"
                                         ("Miskatonic University"
                                         <:> "Miskatonic Museum"
                                         )
                                         ReturnToTheMidnightMasks
                                       )
  { cdCardTraits = setFromList [Arkham]
  }

rivertownAbandonedWarehouse :: CardDef
rivertownAbandonedWarehouse = (location
                                "50030"
                                ("Rivertown" <:> "Abandoned Warehouse")
                                ReturnToTheMidnightMasks
                              )
  { cdCardTraits = setFromList [Arkham, Central]
  }

arkhamWoodsGreatWillow :: CardDef
arkhamWoodsGreatWillow =
  (location "50033" ("Arkham Woods" <:> "Great Willow") ReturnToTheDevourerBelow
    )
    { cdCardTraits = setFromList [Woods]
    }

arkhamWoodsLakeside :: CardDef
arkhamWoodsLakeside =
  (location "50034" ("Arkham Woods" <:> "Lakeside") ReturnToTheDevourerBelow)
    { cdCardTraits = setFromList [Woods]
    }

arkhamWoodsCorpseRiddenClearing :: CardDef
arkhamWoodsCorpseRiddenClearing = (location
                                    "50035"
                                    ("Arkham Woods"
                                    <:> "Corpse-Ridden Clearing"
                                    )
                                    ReturnToTheDevourerBelow
                                  )
  { cdCardTraits = setFromList [Woods]
  }

arkhamWoodsWoodenBridge :: CardDef
arkhamWoodsWoodenBridge =
  (location
      "50036"
      ("Arkham Woods" <:> "Wooden Bridge")
      ReturnToTheDevourerBelow
    )
    { cdCardTraits = setFromList [Woods]
    }

cursedShores :: CardDef
cursedShores = (location "81007" "Cursed Shores" TheBayou)
  { cdCardTraits = setFromList [NewOrleans, Bayou]
  }

gardenDistrict :: CardDef
gardenDistrict = (location "81008" "Garden District" TheBayou)
  { cdCardTraits = setFromList [NewOrleans]
  }

broadmoor :: CardDef
broadmoor = (location "81009" "Broadmoor" TheBayou)
  { cdCardTraits = setFromList [NewOrleans]
  , cdVictoryPoints = Just 1
  }

brackishWaters :: CardDef
brackishWaters = (location "81010" "Brackish Waters" TheBayou)
  { cdCardTraits = setFromList [Riverside, Bayou]
  }

audubonPark :: CardDef
audubonPark = (location "81011" "Audubon Park" TheBayou)
  { cdCardTraits = setFromList [Riverside]
  , cdVictoryPoints = Just 1
  }

fauborgMarigny :: CardDef
fauborgMarigny = (location "81012" "Fauborg Marigny" TheBayou)
  { cdCardTraits = setFromList [Riverside]
  }

forgottenMarsh :: CardDef
forgottenMarsh = (location "81013" "Forgotten Marsh" TheBayou)
  { cdCardTraits = setFromList [Wilderness, Bayou]
  }

trappersCabin :: CardDef
trappersCabin = (location "81014" "Trapper's Cabin" TheBayou)
  { cdCardTraits = setFromList [Wilderness]
  }

twistedUnderbrush :: CardDef
twistedUnderbrush = (location "81015" "Twisted Underbrush" TheBayou)
  { cdCardTraits = setFromList [Wilderness]
  , cdVictoryPoints = Just 1
  }

foulSwamp :: CardDef
foulSwamp = (location "81016" "Foul Swamp" TheBayou)
  { cdCardTraits = setFromList [Unhallowed, Bayou]
  }

ritualGrounds :: CardDef
ritualGrounds = (location "81017" "Ritual Grounds" TheBayou)
  { cdCardTraits = setFromList [Unhallowed]
  , cdVictoryPoints = Just 1
  }

overgrownCairns :: CardDef
overgrownCairns = (location "81018" "Overgrown Cairns" TheBayou)
  { cdCardTraits = setFromList [Unhallowed]
  }
