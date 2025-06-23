module Arkham.Story.Cards where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name
import Arkham.Prelude

doubleSided :: CardDef -> CardDef
doubleSided def =
  def
    { cdDoubleSided = True
    , cdOtherSide = Just $ flippedCardCode def.cardCode
    }

story :: CardCode -> Name -> EncounterSet -> CardDef
story cardCode name encounterSet =
  (emptyCardDef cardCode name StoryType)
    { cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Just 1
    , cdDoubleSided = False
    , cdLevel = Nothing
    }

allStoryCards :: Map CardCode CardDef
allStoryCards =
  mapFromList
    $ map
      (toCardCode &&& id)
      [ sickeningReality_65
      , sickeningReality_66
      , sickeningReality_67
      , sickeningReality_68
      , sickeningReality_69
      , engramsOath
      , lagneauPerdu
      , thePattern
      , theFirstShow
      , aboveAndBelow
      , songsThatTheHyadesShallSing
      , starsOfAldebaran
      , bleakDesolation
      , inhabitantOfCarcosa
      , aMomentsRest
      , theCoffin
      , mappingTheStreets
      , theKingsParade
      , theArchway
      , theHeightOfTheDepths
      , stepsOfThePalace
      , theDelusion
      , theEntity
      , theFall
      , theWriter
      , hastursEnd
      , hastursLastStand
      , yigsMercy
      , anotherWay
      , josefsPlan
      , unfinishedBusiness_B
      , unfinishedBusiness_D
      , unfinishedBusiness_F
      , unfinishedBusiness_H
      , unfinishedBusiness_J
      , unfinishedBusiness_L
      , gavriellasFate
      , jeromesFate
      , pennysFate
      , valentinosFate
      , theTrialOfKamanThah
      , theTrialOfNasht
      , theInfestationBegins
      , crypticSouls
      , dreamlikeHorrors
      , endlessSecrets
      , cylindersOfKadatheron
      , theDoomOfSarnath
      , ghostsOfTheDead
      , thePalaceOfRainbows
      , aShrineToTheGods
      , theCryptOfZulanThek
      , waresOfBaharna
      , theLikenessOfOld
      , whatRemainsOfTyrrhia
      , adviceOfTheKing
      , timelessBeauty
      , unattainableDesires
      , theCityInside
      , theBalefulStar
      , offTheGalley
      , ghastlyTunnels
      , theSentry
      , anotherPath
      , aStrangeGhoul
      , scoutingTheVale
      , somethingBelow
      , inhabitantsOfTheVale
      , theWayOut
      , spiderInfestedWaters
      , stillSurface
      , rollingPits
      , centerOfTheSea
      , findingAgentHarper
      , captured
      , deadEnd
      , cracksInTheIce
      , somberRemains
      , disappearingFootprints
      , dissectedExplorer
      , evilWithin
      , bloodyEvidence
      , madnessInside
      , prisonOfMemories
      , baseCamp
      , deckOfTheTheodosia
      , universityHalls
      , hedgeMaze
      , desertedStation
      , coastalWaters
      , elderChamber
      , riverviewTheatre
      , standingStones
      , airfield
      , alaskanWilds
      , clutteredDormitory
      , dyersClassroom
      , infirmary
      , drKenslersOffice
      , moaiStatues
      , ottomanFront
      , theBlackStone
      , memoryOfAHuntGoneAwry
      , memoryOfALostPatient
      , memoryOfAMissingFather
      , memoryOfARavagedCountry
      , memoryOfARegretfulVoyage
      , memoryOfAnUnspeakableEvil
      , memoryOfATerribleDiscovery
      , memoryOfAnAlienTranslation
      , memoryOfAnUnrequitedLove
      , returnToSickeningReality_23
      , returnToSickeningReality_24
      , realityAcid
      , theFoundationAllied
      , theFoundationRival
      , theSyndicateAllied
      , theSyndicateRival
      , miskatonicUniversityAllied
      , miskatonicUniversityRival
      , silverTwilightLodgeAllied
      , silverTwilightLodgeRival
      , localsOfKingsportAllied
      , localsOfKingsportRival
      ]

victory :: Int -> CardDef -> CardDef
victory n def = def {cdVictoryPoints = Just n}

sickeningReality_65 :: CardDef
sickeningReality_65 = doubleSided $ story "03065" "Sickening Reality" TheLastKing

sickeningReality_66 :: CardDef
sickeningReality_66 = doubleSided $ story "03066" "Sickening Reality" TheLastKing

sickeningReality_67 :: CardDef
sickeningReality_67 = doubleSided $ story "03067" "Sickening Reality" TheLastKing

sickeningReality_68 :: CardDef
sickeningReality_68 = doubleSided $ story "03068" "Sickening Reality" TheLastKing

sickeningReality_69 :: CardDef
sickeningReality_69 = doubleSided $ story "03069" "Sickening Reality" TheLastKing

engramsOath :: CardDef
engramsOath = doubleSided $ story "03076b" "Engram's Oath" TheLastKing

lagneauPerdu :: CardDef
lagneauPerdu = doubleSided $ story "03077b" "L'agneau Perdu" TheLastKing

thePattern :: CardDef
thePattern = doubleSided $ story "03078b" "The Pattern" TheLastKing

theFirstShow :: CardDef
theFirstShow = doubleSided $ story "03079b" "The First Show" TheLastKing

aboveAndBelow :: CardDef
aboveAndBelow = doubleSided $ story "03080b" "Above and Below" TheLastKing

songsThatTheHyadesShallSing :: CardDef
songsThatTheHyadesShallSing =
  doubleSided $ story "03325b" "Songs That the Hyades Shall Sing" DimCarcosa

starsOfAldebaran :: CardDef
starsOfAldebaran = doubleSided $ story "03326b" "Stars of Aldebaran" DimCarcosa

bleakDesolation :: CardDef
bleakDesolation = doubleSided $ story "03326d" "Bleak Desolation" DimCarcosa

inhabitantOfCarcosa :: CardDef
inhabitantOfCarcosa = doubleSided $ story "03327b" "Inhabitant of Carcosa" DimCarcosa

aMomentsRest :: CardDef
aMomentsRest = doubleSided $ story "03327d" "A Moment's Rest" DimCarcosa

theCoffin :: CardDef
theCoffin = doubleSided $ story "03327f" "The Coffin" DimCarcosa

mappingTheStreets :: CardDef
mappingTheStreets = doubleSided $ story "03328b" "Mapping the Streets" DimCarcosa

theKingsParade :: CardDef
theKingsParade = doubleSided $ story "03328d" "The King's Parade" DimCarcosa

theArchway :: CardDef
theArchway = doubleSided $ story "03328f" "The Archway" DimCarcosa

theHeightOfTheDepths :: CardDef
theHeightOfTheDepths = doubleSided $ story "03329b" "The Height of the Depths" DimCarcosa

stepsOfThePalace :: CardDef
stepsOfThePalace = doubleSided $ story "03329d" "Steps of the Palace" DimCarcosa

theFall :: CardDef
theFall = doubleSided $ story "03330b" "The Fall" DimCarcosa

hastursEnd :: CardDef
hastursEnd = doubleSided $ story "03331b" "Hastur's End" DimCarcosa

yigsMercy :: CardDef
yigsMercy = doubleSided $ story "04325b" "Yig's Mercy" ShatteredAeons

anotherWay :: CardDef
anotherWay = doubleSided $ story "04326b" "Another Way" ShatteredAeons

josefsPlan :: CardDef
josefsPlan = doubleSided $ story "05085b" "Josef's Plan" AtDeathsDoorstep

unfinishedBusiness_B :: CardDef
unfinishedBusiness_B = victory 1 $ doubleSided $ story "05178b" "Unfinished Business" TheWagesOfSin

unfinishedBusiness_D :: CardDef
unfinishedBusiness_D = victory 1 $ doubleSided $ story "05178d" "Unfinished Business" TheWagesOfSin

unfinishedBusiness_F :: CardDef
unfinishedBusiness_F = victory 1 $ doubleSided $ story "05178f" "Unfinished Business" TheWagesOfSin

unfinishedBusiness_H :: CardDef
unfinishedBusiness_H = victory 1 $ doubleSided $ story "05178h" "Unfinished Business" TheWagesOfSin

unfinishedBusiness_J :: CardDef
unfinishedBusiness_J = victory 1 $ doubleSided $ story "05178j" "Unfinished Business" TheWagesOfSin

unfinishedBusiness_L :: CardDef
unfinishedBusiness_L = victory 1 $ doubleSided $ story "05178l" "Unfinished Business" TheWagesOfSin

gavriellasFate :: CardDef
gavriellasFate = doubleSided $ story "05262" "Gavriella's Fate" UnionAndDisillusion

jeromesFate :: CardDef
jeromesFate = doubleSided $ story "05263" "Jerome's Fate" UnionAndDisillusion

pennysFate :: CardDef
pennysFate = doubleSided $ story "05264" "Penny's Fate" UnionAndDisillusion

valentinosFate :: CardDef
valentinosFate = doubleSided $ story "05265" "Valentino's Fate" UnionAndDisillusion

theTrialOfKamanThah :: CardDef
theTrialOfKamanThah = doubleSided $ story "06057b" "The Trial of Kaman-Thah" BeyondTheGatesOfSleep

theTrialOfNasht :: CardDef
theTrialOfNasht = doubleSided $ story "06058b" "The Trial of Nasht" BeyondTheGatesOfSleep

theInfestationBegins :: CardDef
theInfestationBegins = doubleSided $ story "06078" "The Infestation Begins" WakingNightmare

crypticSouls :: CardDef
crypticSouls = doubleSided $ story "06127b" "Cryptic Souls" TheSearchForKadath

dreamlikeHorrors :: CardDef
dreamlikeHorrors = doubleSided $ story "06128b" "Dreamlike Horrors" TheSearchForKadath

endlessSecrets :: CardDef
endlessSecrets = doubleSided $ story "06129b" "Endless Secrets" TheSearchForKadath

cylindersOfKadatheron :: CardDef
cylindersOfKadatheron = doubleSided $ story "06130b" "Cylinders of Kadatheron" TheSearchForKadath

theDoomOfSarnath :: CardDef
theDoomOfSarnath = doubleSided $ story "06131b" "The Doom of Sarnath" TheSearchForKadath

ghostsOfTheDead :: CardDef
ghostsOfTheDead = doubleSided $ story "06132b" "Ghosts of the Dead" TheSearchForKadath

thePalaceOfRainbows :: CardDef
thePalaceOfRainbows = doubleSided $ story "06133b" "The Palace of Rainbows" TheSearchForKadath

aShrineToTheGods :: CardDef
aShrineToTheGods = doubleSided $ story "06134b" "A Shrine to the Gods" TheSearchForKadath

theCryptOfZulanThek :: CardDef
theCryptOfZulanThek = doubleSided $ story "06135b" "The Crypt of Zulan-Thek" TheSearchForKadath

waresOfBaharna :: CardDef
waresOfBaharna = doubleSided $ story "06136b" "Wares of Baharna" TheSearchForKadath

theLikenessOfOld :: CardDef
theLikenessOfOld = doubleSided $ story "06137b" "The Likeness of Old" TheSearchForKadath

whatRemainsOfTyrrhia :: CardDef
whatRemainsOfTyrrhia = doubleSided $ story "06138b" "What Remains of Tyrrhia" TheSearchForKadath

adviceOfTheKing :: CardDef
adviceOfTheKing = doubleSided $ story "06139b" "Advice of the King" TheSearchForKadath

timelessBeauty :: CardDef
timelessBeauty = doubleSided $ story "06140b" "Timeless Beauty" TheSearchForKadath

unattainableDesires :: CardDef
unattainableDesires = doubleSided $ story "06141b" "Unattainable Desires" TheSearchForKadath

theCityInside :: CardDef
theCityInside = doubleSided $ story "06142b" "The City Inside" TheSearchForKadath

theBalefulStar :: CardDef
theBalefulStar = doubleSided $ story "06143b" "The Baleful Star" TheSearchForKadath

offTheGalley :: CardDef
offTheGalley = doubleSided $ story "06214b" "Off the Galley" DarkSideOfTheMoon

ghastlyTunnels :: CardDef
ghastlyTunnels = doubleSided $ story "06254b" "Ghastly Tunnels" PointOfNoReturn

theSentry :: CardDef
theSentry = doubleSided $ story "06255b" "The Sentry" PointOfNoReturn

anotherPath :: CardDef
anotherPath = doubleSided $ story "06256b" "Another Path" PointOfNoReturn

aStrangeGhoul :: CardDef
aStrangeGhoul = doubleSided $ story "06257b" "A Strange Ghoul" PointOfNoReturn

scoutingTheVale :: CardDef
scoutingTheVale = doubleSided $ story "06258b" "Scouting the Vale" PointOfNoReturn

somethingBelow :: CardDef
somethingBelow = doubleSided $ story "06259b" "Something Below" PointOfNoReturn

inhabitantsOfTheVale :: CardDef
inhabitantsOfTheVale = doubleSided $ story "06260b" "Inhabitants of the Vale" PointOfNoReturn

theWayOut :: CardDef
theWayOut = doubleSided $ story "06261b" "The Way Out" PointOfNoReturn

spiderInfestedWaters :: CardDef
spiderInfestedWaters = doubleSided $ story "06262b" "Spider-Infested Waters" PointOfNoReturn

stillSurface :: CardDef
stillSurface = doubleSided $ story "06263b" "Still Surface" PointOfNoReturn

rollingPits :: CardDef
rollingPits = doubleSided $ story "06264b" "Rolling Pits" PointOfNoReturn

centerOfTheSea :: CardDef
centerOfTheSea = doubleSided $ story "06265b" "Center of the Sea" PointOfNoReturn

findingAgentHarper :: CardDef
findingAgentHarper = doubleSided $ story "07062" "Finding Agent Harper" TheVanishingOfElinaHarper

captured :: CardDef
captured = doubleSided $ story "07252" "Captured!" ALightInTheFog

deadEnd :: CardDef
deadEnd = story "08527" "Dead End" LostInTheNight

cracksInTheIce :: CardDef
cracksInTheIce = story "08528" "Cracks in the Ice" LostInTheNight

somberRemains :: CardDef
somberRemains = story "08529" "Somber Remains" LostInTheNight

disappearingFootprints :: CardDef
disappearingFootprints = story "08530" "Disappearing Footprints" LostInTheNight

dissectedExplorer :: CardDef
dissectedExplorer = story "08531" "Dissected Explorer" LostInTheNight

evilWithin :: CardDef
evilWithin = story "08532" "Evil Within" LostInTheNight

bloodyEvidence :: CardDef
bloodyEvidence = story "08533" "Bloody Evidence" LostInTheNight

madnessInside :: CardDef
madnessInside = story "08534" "Madness Inside" LostInTheNight

prisonOfMemories :: CardDef
prisonOfMemories = doubleSided $ story "08556b" "Prison of Memories" FatalMirage

baseCamp :: CardDef
baseCamp = doubleSided $ story "08557b" "Base Camp" FatalMirage

deckOfTheTheodosia :: CardDef
deckOfTheTheodosia = doubleSided $ story "08558b" "Deck of the Theodosia" FatalMirage

universityHalls :: CardDef
universityHalls = doubleSided $ story "08559b" "University Halls" FatalMirage

hedgeMaze :: CardDef
hedgeMaze = doubleSided $ story "08560b" "Hedge Maze" FatalMirage

desertedStation :: CardDef
desertedStation = doubleSided $ story "08561b" "Deserted Station" FatalMirage

coastalWaters :: CardDef
coastalWaters = doubleSided $ story "08562b" "Coastal Waters" FatalMirage

elderChamber :: CardDef
elderChamber = doubleSided $ story "08563b" "Elder Chamber" FatalMirage

riverviewTheatre :: CardDef
riverviewTheatre = doubleSided $ story "08564b" "Riverview Theatre" FatalMirage

standingStones :: CardDef
standingStones = doubleSided $ story "08565b" "Standing Stones" FatalMirage

airfield :: CardDef
airfield = victory 2 $ doubleSided $ story "08566b" "Airfield" FatalMirage

alaskanWilds :: CardDef
alaskanWilds = victory 2 $ doubleSided $ story "08567b" "Alaskan Wilds" FatalMirage

clutteredDormitory :: CardDef
clutteredDormitory = victory 2 $ doubleSided $ story "08568b" "Cluttered Dormitory" FatalMirage

dyersClassroom :: CardDef
dyersClassroom = victory 2 $ doubleSided $ story "08569b" "Dyer's Classroom" FatalMirage

infirmary :: CardDef
infirmary = victory 2 $ doubleSided $ story "08570b" "Infirmary" FatalMirage

drKenslersOffice :: CardDef
drKenslersOffice = victory 2 $ doubleSided $ story "08571b" "Dr. Kensler's Office" FatalMirage

moaiStatues :: CardDef
moaiStatues = victory 2 $ doubleSided $ story "08572b" "Mo'ai Statues" FatalMirage

ottomanFront :: CardDef
ottomanFront = victory 2 $ doubleSided $ story "08573b" "Ottoman Front" FatalMirage

theBlackStone :: CardDef
theBlackStone = victory 2 $ doubleSided $ story "08574b" "The Black Stone" FatalMirage

memoryOfAHuntGoneAwry :: CardDef
memoryOfAHuntGoneAwry =
  victory 1 $ doubleSided $ story "08575b" "Memory of a Hunt Gone Awry" FatalMirage

memoryOfALostPatient :: CardDef
memoryOfALostPatient = victory 1 $ doubleSided $ story "08576b" "Memory of a Lost Patient" FatalMirage

memoryOfAMissingFather :: CardDef
memoryOfAMissingFather = victory 1 $ doubleSided $ story "08577b" "Memory of a Missing Father" FatalMirage

memoryOfARavagedCountry :: CardDef
memoryOfARavagedCountry = victory 1 $ doubleSided $ story "08578b" "Memory of a Ravaged Country" FatalMirage

memoryOfARegretfulVoyage :: CardDef
memoryOfARegretfulVoyage = victory 1 $ doubleSided $ story "08579b" "Memory of a Regretful Voyage" FatalMirage

memoryOfAnUnspeakableEvil :: CardDef
memoryOfAnUnspeakableEvil = victory 1 $ doubleSided $ story "08580b" "Memory of an Unspeakable Evil" FatalMirage

memoryOfATerribleDiscovery :: CardDef
memoryOfATerribleDiscovery = victory 1 $ doubleSided $ story "08581b" "Memory of a Terrible Discovery" FatalMirage

memoryOfAnAlienTranslation :: CardDef
memoryOfAnAlienTranslation = victory 1 $ doubleSided $ story "08582b" "Memory of an Alien Transformation" FatalMirage

memoryOfAnUnrequitedLove :: CardDef
memoryOfAnUnrequitedLove = victory 1 $ doubleSided $ story "08583b" "Memory of an Unrequited Love" FatalMirage

returnToSickeningReality_23 :: CardDef
returnToSickeningReality_23 = doubleSided $ story "52023" "Sickening Reality" ReturnToTheLastKing

returnToSickeningReality_24 :: CardDef
returnToSickeningReality_24 = doubleSided $ story "52024" "Sickening Reality" ReturnToTheLastKing

hastursLastStand :: CardDef
hastursLastStand = doubleSided $ story "52060b" "Hastur's Last Stand" ReturnToDimCarcosa

theWriter :: CardDef
theWriter = doubleSided $ story "52061b" "The Writer" ReturnToDimCarcosa

theEntity :: CardDef
theEntity = doubleSided $ story "52062b" "The Entity" ReturnToDimCarcosa

theDelusion :: CardDef
theDelusion = doubleSided $ story "52063b" "The Delusion" ReturnToDimCarcosa

realityAcid :: CardDef
realityAcid =
  doubleSided
    $ (story "89005" "Reality Acid" TheBlobThatAteEverythingELSE)
      { cdEncounterSet = Nothing
      , cdEncounterSetQuantity = Nothing
      }

theFoundationAllied :: CardDef
theFoundationAllied = doubleSided $ story "71015" "The Foundation [guardian]" TheMidwinterGala

theFoundationRival :: CardDef
theFoundationRival = doubleSided $ story "71015b" "The Foundation [guardian]" TheMidwinterGala

miskatonicUniversityAllied :: CardDef
miskatonicUniversityAllied = doubleSided $ story "71021" "Miskatonic University [seeker]" TheMidwinterGala

miskatonicUniversityRival :: CardDef
miskatonicUniversityRival = doubleSided $ story "71021b" "Miskatonic University [seeker]" TheMidwinterGala

theSyndicateAllied :: CardDef
theSyndicateAllied = doubleSided $ story "71027" "The Syndicate [rogue]" TheMidwinterGala

theSyndicateRival :: CardDef
theSyndicateRival = doubleSided $ story "71027b" "The Syndicate [rogue]" TheMidwinterGala

silverTwilightLodgeAllied :: CardDef
silverTwilightLodgeAllied = doubleSided $ story "71033" "Silver Twilight Lodge [mystic]" TheMidwinterGala

silverTwilightLodgeRival :: CardDef
silverTwilightLodgeRival = doubleSided $ story "71033b" "Silver Twilight Lodge [mystic]" TheMidwinterGala

localsOfKingsportAllied :: CardDef
localsOfKingsportAllied = doubleSided $ story "71039" "Locals of Kingsport [survivor]" TheMidwinterGala

localsOfKingsportRival :: CardDef
localsOfKingsportRival = doubleSided $ story "71039b" "Locals of Kingsport [survivor]" TheMidwinterGala
