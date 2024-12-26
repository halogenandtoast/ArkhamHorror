module Arkham.Story.Cards where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name

doubleSided :: CardCode -> CardDef -> CardDef
doubleSided cCode def =
  def
    { cdDoubleSided = True
    , cdOtherSide = Just cCode
    }

story :: CardCode -> Name -> EncounterSet -> CardDef
story cardCode name encounterSet =
  (emptyCardDef cardCode name StoryType)
    { cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Just 1
    , cdDoubleSided = True
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
      , theFall
      , hastursEnd
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
      , realityAcid
      ]

victory :: Int -> CardDef -> CardDef
victory n def = def {cdVictoryPoints = Just n}

sickeningReality_65 :: CardDef
sickeningReality_65 = story "03065" "Sickening Reality" TheLastKing

sickeningReality_66 :: CardDef
sickeningReality_66 = story "03066" "Sickening Reality" TheLastKing

sickeningReality_67 :: CardDef
sickeningReality_67 = story "03067" "Sickening Reality" TheLastKing

sickeningReality_68 :: CardDef
sickeningReality_68 = story "03068" "Sickening Reality" TheLastKing

sickeningReality_69 :: CardDef
sickeningReality_69 = story "03069" "Sickening Reality" TheLastKing

engramsOath :: CardDef
engramsOath = story "03076b" "Engram's Oath" TheLastKing

lagneauPerdu :: CardDef
lagneauPerdu = story "03077b" "L'agneau Perdu" TheLastKing

thePattern :: CardDef
thePattern = story "03078b" "The Pattern" TheLastKing

theFirstShow :: CardDef
theFirstShow = story "03079b" "The First Show" TheLastKing

aboveAndBelow :: CardDef
aboveAndBelow = story "03080b" "Above and Below" TheLastKing

songsThatTheHyadesShallSing :: CardDef
songsThatTheHyadesShallSing =
  story "03325b" "Songs That the Hyades Shall Sing" DimCarcosa

starsOfAldebaran :: CardDef
starsOfAldebaran = story "03326b" "Stars of Aldebaran" DimCarcosa

bleakDesolation :: CardDef
bleakDesolation = story "03326d" "Bleak Desolation" DimCarcosa

inhabitantOfCarcosa :: CardDef
inhabitantOfCarcosa = story "03327b" "Inhabitant of Carcosa" DimCarcosa

aMomentsRest :: CardDef
aMomentsRest = story "03327d" "A Moment's Rest" DimCarcosa

theCoffin :: CardDef
theCoffin = story "03327f" "The Coffin" DimCarcosa

mappingTheStreets :: CardDef
mappingTheStreets = story "03328b" "Mapping the Streets" DimCarcosa

theKingsParade :: CardDef
theKingsParade = story "03328d" "The King's Parade" DimCarcosa

theArchway :: CardDef
theArchway = story "03328f" "The Archway" DimCarcosa

theHeightOfTheDepths :: CardDef
theHeightOfTheDepths = story "03329b" "The Height of the Depths" DimCarcosa

stepsOfThePalace :: CardDef
stepsOfThePalace = story "03329d" "Steps of the Palace" DimCarcosa

theFall :: CardDef
theFall = story "03330b" "The Fall" DimCarcosa

hastursEnd :: CardDef
hastursEnd = story "03331b" "Hastur's End" DimCarcosa

yigsMercy :: CardDef
yigsMercy = story "04325b" "Yig's Mercy" ShatteredAeons

anotherWay :: CardDef
anotherWay = story "04326b" "Another Way" ShatteredAeons

josefsPlan :: CardDef
josefsPlan = story "05085b" "Josef's Plan" AtDeathsDoorstep

unfinishedBusiness_B :: CardDef
unfinishedBusiness_B = victory 1 $ story "05178b" "Unfinished Business" TheWagesOfSin

unfinishedBusiness_D :: CardDef
unfinishedBusiness_D = victory 1 $ story "05178d" "Unfinished Business" TheWagesOfSin

unfinishedBusiness_F :: CardDef
unfinishedBusiness_F = victory 1 $ story "05178f" "Unfinished Business" TheWagesOfSin

unfinishedBusiness_H :: CardDef
unfinishedBusiness_H = victory 1 $ story "05178h" "Unfinished Business" TheWagesOfSin

unfinishedBusiness_J :: CardDef
unfinishedBusiness_J = victory 1 $ story "05178j" "Unfinished Business" TheWagesOfSin

unfinishedBusiness_L :: CardDef
unfinishedBusiness_L = victory 1 $ story "05178l" "Unfinished Business" TheWagesOfSin

gavriellasFate :: CardDef
gavriellasFate = story "05262" "Gavriella's Fate" UnionAndDisillusion

jeromesFate :: CardDef
jeromesFate = story "05263" "Jerome's Fate" UnionAndDisillusion

pennysFate :: CardDef
pennysFate = story "05264" "Penny's Fate" UnionAndDisillusion

valentinosFate :: CardDef
valentinosFate = story "05265" "Valentino's Fate" UnionAndDisillusion

theTrialOfKamanThah :: CardDef
theTrialOfKamanThah = story "06057b" "The Trial of Kaman-Thah" BeyondTheGatesOfSleep

theTrialOfNasht :: CardDef
theTrialOfNasht = story "06058b" "The Trial of Nasht" BeyondTheGatesOfSleep

theInfestationBegins :: CardDef
theInfestationBegins = story "06078" "The Infestation Begins" WakingNightmare

crypticSouls :: CardDef
crypticSouls = story "06127b" "Cryptic Souls" TheSearchForKadath

dreamlikeHorrors :: CardDef
dreamlikeHorrors = story "06128b" "Dreamlike Horrors" TheSearchForKadath

endlessSecrets :: CardDef
endlessSecrets = story "06129b" "Endless Secrets" TheSearchForKadath

cylindersOfKadatheron :: CardDef
cylindersOfKadatheron = story "06130b" "Cylinders of Kadatheron" TheSearchForKadath

theDoomOfSarnath :: CardDef
theDoomOfSarnath = story "06131b" "The Doom of Sarnath" TheSearchForKadath

ghostsOfTheDead :: CardDef
ghostsOfTheDead = story "06132b" "Ghosts of the Dead" TheSearchForKadath

thePalaceOfRainbows :: CardDef
thePalaceOfRainbows = story "06133b" "The Palace of Rainbows" TheSearchForKadath

aShrineToTheGods :: CardDef
aShrineToTheGods = story "06134b" "A Shrine to the Gods" TheSearchForKadath

theCryptOfZulanThek :: CardDef
theCryptOfZulanThek = story "06135b" "The Crypt of Zulan-Thek" TheSearchForKadath

waresOfBaharna :: CardDef
waresOfBaharna = story "06136b" "Wares of Baharna" TheSearchForKadath

theLikenessOfOld :: CardDef
theLikenessOfOld = story "06137b" "The Likeness of Old" TheSearchForKadath

whatRemainsOfTyrrhia :: CardDef
whatRemainsOfTyrrhia = story "06138b" "What Remains of Tyrrhia" TheSearchForKadath

adviceOfTheKing :: CardDef
adviceOfTheKing = story "06139b" "Advice of the King" TheSearchForKadath

timelessBeauty :: CardDef
timelessBeauty = story "06140b" "Timeless Beauty" TheSearchForKadath

unattainableDesires :: CardDef
unattainableDesires = story "06141b" "Unattainable Desires" TheSearchForKadath

theCityInside :: CardDef
theCityInside = story "06142b" "The City Inside" TheSearchForKadath

theBalefulStar :: CardDef
theBalefulStar = story "06143b" "The Baleful Star" TheSearchForKadath

offTheGalley :: CardDef
offTheGalley = story "06214b" "Off the Galley" DarkSideOfTheMoon

ghastlyTunnels :: CardDef
ghastlyTunnels = story "06254b" "Ghastly Tunnels" PointOfNoReturn

theSentry :: CardDef
theSentry = story "06255b" "The Sentry" PointOfNoReturn

anotherPath :: CardDef
anotherPath = story "06256b" "Another Path" PointOfNoReturn

aStrangeGhoul :: CardDef
aStrangeGhoul = story "06257b" "A Strange Ghoul" PointOfNoReturn

scoutingTheVale :: CardDef
scoutingTheVale = story "06258b" "Scouting the Vale" PointOfNoReturn

somethingBelow :: CardDef
somethingBelow = story "06259b" "Something Below" PointOfNoReturn

inhabitantsOfTheVale :: CardDef
inhabitantsOfTheVale = story "06260b" "Inhabitants of the Vale" PointOfNoReturn

theWayOut :: CardDef
theWayOut = story "06261b" "The Way Out" PointOfNoReturn

spiderInfestedWaters :: CardDef
spiderInfestedWaters = story "06262b" "Spider-Infested Waters" PointOfNoReturn

stillSurface :: CardDef
stillSurface = story "06263b" "Still Surface" PointOfNoReturn

rollingPits :: CardDef
rollingPits = story "06264b" "Rolling Pits" PointOfNoReturn

centerOfTheSea :: CardDef
centerOfTheSea = story "06265b" "Center of the Sea" PointOfNoReturn

findingAgentHarper :: CardDef
findingAgentHarper = story "07062" "Finding Agent Harper" TheVanishingOfElinaHarper

captured :: CardDef
captured = story "07252" "Captured!" ALightInTheFog

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
prisonOfMemories = doubleSided "08556" $ story "08556b" "Prison of Memories" FatalMirage

baseCamp :: CardDef
baseCamp = doubleSided "08557" $ story "08557b" "Base Camp" FatalMirage

deckOfTheTheodosia :: CardDef
deckOfTheTheodosia = doubleSided "08558" $ story "08558b" "Deck of the Theodosia" FatalMirage

universityHalls :: CardDef
universityHalls = doubleSided "08559" $ story "08559b" "University Halls" FatalMirage

hedgeMaze :: CardDef
hedgeMaze = doubleSided "08560" $ story "08560b" "Hedge Maze" FatalMirage

desertedStation :: CardDef
desertedStation = doubleSided "08561" $ story "08561b" "Deserted Station" FatalMirage

coastalWaters :: CardDef
coastalWaters = doubleSided "08562" $ story "08562b" "Coastal Waters" FatalMirage

elderChamber :: CardDef
elderChamber = doubleSided "08563" $ story "08563b" "Elder Chamber" FatalMirage

riverviewTheatre :: CardDef
riverviewTheatre = doubleSided "08564" $ story "08564b" "Riverview Theatre" FatalMirage

standingStones :: CardDef
standingStones = doubleSided "08565" $ story "08565b" "Standing Stones" FatalMirage

airfield :: CardDef
airfield = doubleSided "08566" $ story "08566b" "Airfield" FatalMirage

alaskanWilds :: CardDef
alaskanWilds = doubleSided "08567" $ story "08567b" "Alaskan Wilds" FatalMirage

clutteredDormitory :: CardDef
clutteredDormitory = doubleSided "08568" $ story "08568b" "Cluttered Dormitory" FatalMirage

dyersClassroom :: CardDef
dyersClassroom = doubleSided "08569" $ story "08569b" "Dyer's Classroom" FatalMirage

infirmary :: CardDef
infirmary = doubleSided "08570" $ story "08570b" "Infirmary" FatalMirage

drKenslersOffice :: CardDef
drKenslersOffice = doubleSided "08571" $ story "08571b" "Dr. Kensler's Office" FatalMirage

moaiStatues :: CardDef
moaiStatues = doubleSided "08572" $ story "08572b" "Mo'ai Statues" FatalMirage

ottomanFront :: CardDef
ottomanFront = doubleSided "08573" $ story "08573b" "Ottoman Front" FatalMirage

theBlackStone :: CardDef
theBlackStone = doubleSided "08574" $ story "08574b" "The Black Stone" FatalMirage

memoryOfAHuntGoneAwry :: CardDef
memoryOfAHuntGoneAwry =
  doubleSided "08575" $ story "08575b" "Memory of a Hunt Gone Awry" FatalMirage

memoryOfALostPatient :: CardDef
memoryOfALostPatient = doubleSided "08576" $ story "08576b" "Memory of a Lost Patient" FatalMirage

memoryOfAMissingFather :: CardDef
memoryOfAMissingFather = doubleSided "08577" $ story "08577b" "Memory of a Missing Father" FatalMirage

memoryOfARavagedCountry :: CardDef
memoryOfARavagedCountry = doubleSided "08578" $ story "08578b" "Memory of a Ravaged Country" FatalMirage

memoryOfARegretfulVoyage :: CardDef
memoryOfARegretfulVoyage = doubleSided "08579" $ story "08579b" "Memory of a Regretful Voyage" FatalMirage

memoryOfAnUnspeakableEvil :: CardDef
memoryOfAnUnspeakableEvil = doubleSided "08580" $ story "08580b" "Memory of an Unspeakable Evil" FatalMirage

memoryOfATerribleDiscovery :: CardDef
memoryOfATerribleDiscovery = doubleSided "08581" $ story "08581b" "Memory of a Terrible Discovery" FatalMirage

memoryOfAnAlienTranslation :: CardDef
memoryOfAnAlienTranslation = doubleSided "08582" $ story "08582b" "Memory of an Alien Transformation" FatalMirage

memoryOfAnUnrequitedLove :: CardDef
memoryOfAnUnrequitedLove = doubleSided "08583" $ story "08583b" "Memory of an Unrequited Love" FatalMirage

realityAcid :: CardDef
realityAcid =
  (story "89005" "Reality Acid" TheBlobThatAteEverythingELSE)
    { cdEncounterSet = Nothing
    , cdEncounterSetQuantity = Nothing
    }
