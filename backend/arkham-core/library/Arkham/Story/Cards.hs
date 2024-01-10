module Arkham.Story.Cards where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name

story :: CardCode -> Name -> EncounterSet -> CardDef
story cardCode name encounterSet =
  (emptyCardDef cardCode name StoryType)
    { cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Just 1
    , cdDoubleSided = True
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
