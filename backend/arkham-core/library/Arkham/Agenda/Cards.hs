module Arkham.Agenda.Cards where

import Arkham.Prelude hiding (fold)

import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name

agenda :: CardCode -> Name -> Int -> EncounterSet -> CardDef
agenda cardCode name stage encounterSet =
  CardDef
    { cdCardCode = cardCode
    , cdName = name
    , cdRevealedName = Nothing
    , cdCost = Nothing
    , cdAdditionalCost = Nothing
    , cdLevel = 0
    , cdCardType = AgendaType
    , cdCardSubType = Nothing
    , cdClassSymbols = mempty
    , cdSkills = mempty
    , cdCardTraits = mempty
    , cdRevealedCardTraits = mempty
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
    , cdEncounterSetQuantity = Nothing
    , cdUnique = False
    , cdDoubleSided = True
    , cdLimits = []
    , cdExceptional = False
    , cdUses = NoUses
    , cdPlayableFromDiscard = False
    , cdStage = Just stage
    , cdSlots = []
    , cdCardInHandEffects = False
    , cdCardInDiscardEffects = False
    , cdCardInSearchEffects = False
    , cdAlternateCardCodes = []
    , cdArt = unCardCode cardCode
    , cdLocationSymbol = Nothing
    , cdLocationRevealedSymbol = Nothing
    , cdLocationConnections = []
    , cdLocationRevealedConnections = []
    , cdPurchaseMentalTrauma = Nothing
    , cdCanReplace = True
    , cdDeckRestrictions = []
    }

allAgendaCards :: Map CardCode CardDef
allAgendaCards =
  mapFromList
    $ map
      (toCardCode &&& id)
      [ aCreatureOfTheBayou
      , aTearInReality
      , allIsOne
      , beckoningForPower
      , behindTheCurtain
      , bidingItsTime
      , breakingThrough
      , callingForthTheOldOnes
      , chaosAtTheCarnevale
      , chaosInTheCloverClub
      , cityOfBlood
      , cityOfTheGreatRace
      , crossroadsOfFate
      , deadOfNight
      , deathsApproach
      , drawnIn
      , empireOfTheDead
      , empireOfTheUndead
      , encore
      , endlessCaverns
      , endsAndMeans
      , expeditionIntoTheWild
      , eyesInTheDark
      , fashionablyLate
      , feedTheBeast
      , furyThatShakesTheEarth
      , hiddenEntanglements
      , hisDomain
      , horrificDescent
      , horrorsUnleashed
      , humanityFading
      , intruders
      , inEveryShadow
      , itAwaits
      , judgementXX
      , justiceXI
      , letTheStormRageTheFloodBelow
      , letTheStormRageTheVortexAbove
      , lockedInside
      , lostMemories
      , madnessCoils
      , madnessDies
      , madnessDrowns
      , markedForSacrifice
      , outOfTime
      , overTheThreshold
      , pastPresentAndFuture
      , pendulousThreads
      , predatorOrPrey
      , quietHalls
      , rampagingCreatures
      , ransackingTheManor
      , restrictedAccess
      , returnToPredatorOrPrey
      , riseOfTheGhouls
      , rollingBackwards
      , secretsBetterLeftHidden
      , settingSun
      , shadowsDeepen
      , snappedThreads
      , somethingStirs
      , strangeDisappearances
      , swallowedSky
      , temperanceXIV
      , theArkhamWoods
      , theBarrierIsThin
      , theBeastUnleashed
      , theBoundaryBroken
      , theChariotVII
      , theCityFloods
      , theCloverClub
      , theCurseSpreads
      , theDescentBegins
      , theEndOfAllThings
      , theEntityAboveTheFloodBelow
      , theEntityAboveTheVortexAbove
      , theFamiliar
      , theFestivitiesBegin
      , theFinalCountdown
      , theFirstNight
      , theHangedManXII
      , theHermitIX
      , theHierophantV
      , theJunglesHeart
      , theLonelyCaverns
      , theLoversVI
      , theMawWidens
      , theNightHowls
      , theOldOnesHunger
      , theRedDepths
      , theRitualBegins
      , theRitualBeginsBlackStarsRise
      , theRougarouFeeds
      , theSecondNight
      , theShadowOfTheEclipse
      , theTempleWarden
      , theTerrifyingTruth
      , theThirdAct
      , theThirdNight
      , theTideRises
      , theTruthIsHidden
      , theWitchLight
      , theyreGettingOut
      , threadsOfTime
      , threeFates
      , timeCollapsing
      , timeIsRunningShort
      , torturousDescent
      , undergroundMuscle
      , vengeance
      , vengeanceAwaits
      , whatsGoingOn
      , wheelOfFortuneX
      ]

whatsGoingOn :: CardDef
whatsGoingOn = agenda "01105" "What's Going On?!" 1 TheGathering

riseOfTheGhouls :: CardDef
riseOfTheGhouls = agenda "01106" "Rise of the Ghouls" 2 TheGathering

theyreGettingOut :: CardDef
theyreGettingOut = agenda "01107" "They're Getting Out!" 3 TheGathering

predatorOrPrey :: CardDef
predatorOrPrey = agenda "01121" "Predator or Prey?" 1 TheMidnightMasks

timeIsRunningShort :: CardDef
timeIsRunningShort = agenda "01122" "Time Is Running Short" 2 TheMidnightMasks

theArkhamWoods :: CardDef
theArkhamWoods = agenda "01143" "The Arkham Woods" 1 TheDevourerBelow

theRitualBegins :: CardDef
theRitualBegins = agenda "01144" "The Ritual Begins" 2 TheDevourerBelow

vengeanceAwaits :: CardDef
vengeanceAwaits = agenda "01145" "Vengeance Awaits" 3 TheDevourerBelow

quietHalls :: CardDef
quietHalls = agenda "02042" "Quiet Halls" 1 ExtracurricularActivity

deadOfNight :: CardDef
deadOfNight = agenda "02043" "Dead of Night" 2 ExtracurricularActivity

theBeastUnleashed :: CardDef
theBeastUnleashed =
  agenda "02044" "The Beast Unleashed" 3 ExtracurricularActivity

theCloverClub :: CardDef
theCloverClub = agenda "02063" "The Clover Club" 1 TheHouseAlwaysWins

undergroundMuscle :: CardDef
undergroundMuscle = agenda "02064" "Underground Muscle" 2 TheHouseAlwaysWins

chaosInTheCloverClub :: CardDef
chaosInTheCloverClub =
  agenda "02065" "Chaos in the Clover Club" 3 TheHouseAlwaysWins

restrictedAccess :: CardDef
restrictedAccess = agenda "02119" "Restricted Access" 1 TheMiskatonicMuseum

shadowsDeepen :: CardDef
shadowsDeepen = agenda "02120" "Shadows Deepen" 2 TheMiskatonicMuseum

inEveryShadow :: CardDef
inEveryShadow = agenda "02121" "In Every Shadow" 3 TheMiskatonicMuseum

aTearInReality :: CardDef
aTearInReality = agenda "02160" "A Tear in Reality" 1 TheEssexCountyExpress

theMawWidens :: CardDef
theMawWidens = agenda "02161" "The Maw Widens" 2 TheEssexCountyExpress

rollingBackwards :: CardDef
rollingBackwards = agenda "02162" "Rolling Backwards" 3 TheEssexCountyExpress

drawnIn :: CardDef
drawnIn = agenda "02163" "Drawn In" 4 TheEssexCountyExpress

outOfTime :: CardDef
outOfTime = agenda "02164" "Out of Time" 5 TheEssexCountyExpress

strangeDisappearances :: CardDef
strangeDisappearances =
  agenda "02196" "Strange Disappearances" 1 BloodOnTheAltar

theOldOnesHunger :: CardDef
theOldOnesHunger = agenda "02197" "The Old Ones Hunger" 2 BloodOnTheAltar

feedTheBeast :: CardDef
feedTheBeast = agenda "02198" "Feed the Beast" 3 BloodOnTheAltar

rampagingCreatures :: CardDef
rampagingCreatures =
  agenda "02237" "Rampaging Creatures" 1 UndimensionedAndUnseen

bidingItsTime :: CardDef
bidingItsTime = agenda "02238" "Biding Its Time" 2 UndimensionedAndUnseen

horrorsUnleashed :: CardDef
horrorsUnleashed = agenda "02239" "Horrors Unleashed" 3 UndimensionedAndUnseen

callingForthTheOldOnes :: CardDef
callingForthTheOldOnes =
  agenda "02275" "Calling Forth the Old Ones" 1 WhereDoomAwaits

beckoningForPower :: CardDef
beckoningForPower = agenda "02276" "Beckoning for Power" 2 WhereDoomAwaits

allIsOne :: CardDef
allIsOne = agenda "02312" "All is One" 1 LostInTimeAndSpace

pastPresentAndFuture :: CardDef
pastPresentAndFuture =
  agenda "02313" "Past, Present and Future" 2 LostInTimeAndSpace

breakingThrough :: CardDef
breakingThrough = agenda "02314" "Breaking Through" 3 LostInTimeAndSpace

theEndOfAllThings :: CardDef
theEndOfAllThings = agenda "02315" "The End of All Things" 4 LostInTimeAndSpace

theThirdAct :: CardDef
theThirdAct = agenda "03044" "The Third Act" 1 CurtainCall

encore :: CardDef
encore = agenda "03045" "Encore" 2 CurtainCall

fashionablyLate :: CardDef
fashionablyLate = agenda "03062" "Fashionably Late" 1 TheLastKing

theTerrifyingTruth :: CardDef
theTerrifyingTruth = agenda "03063" "The Terrifying Truth" 2 TheLastKing

theTruthIsHidden :: CardDef
theTruthIsHidden = agenda "03121" "The Truth is Hidden" 1 EchoesOfThePast

ransackingTheManor :: CardDef
ransackingTheManor = agenda "03122" "Ransacking the Manor" 2 EchoesOfThePast

secretsBetterLeftHidden :: CardDef
secretsBetterLeftHidden =
  agenda "03123" "Secrets Better Left Hidden" 3 EchoesOfThePast

lockedInside :: CardDef
lockedInside = agenda "03160" "Locked Inside" 1 TheUnspeakableOath

torturousDescent :: CardDef
torturousDescent = agenda "03161" "Torturous Descent" 2 TheUnspeakableOath

hisDomain :: CardDef
hisDomain = agenda "03162" "His Domain" 3 TheUnspeakableOath

theFirstNight :: CardDef
theFirstNight = agenda "03201" "The First Night" 1 APhantomOfTruth

theSecondNight :: CardDef
theSecondNight = agenda "03202" "The Second Night" 2 APhantomOfTruth

theThirdNight :: CardDef
theThirdNight = agenda "03203" "The Third Night" 3 APhantomOfTruth

empireOfTheDead :: CardDef
empireOfTheDead = agenda "03241" "Empire of the Dead" 1 ThePallidMask

empireOfTheUndead :: CardDef
empireOfTheUndead = agenda "03242" "Empire of the Undead" 2 ThePallidMask

theTideRises :: CardDef
theTideRises = agenda "03275" "The Tide Rises" 1 BlackStarsRise

letTheStormRageTheFloodBelow :: CardDef
letTheStormRageTheFloodBelow = agenda "03276a" "Let The Storm Rage" 2 BlackStarsRise

letTheStormRageTheVortexAbove :: CardDef
letTheStormRageTheVortexAbove = agenda "03276b" "Let The Storm Rage" 2 BlackStarsRise

theCityFloods :: CardDef
theCityFloods = agenda "03277" "The City Floods" 3 BlackStarsRise

theRitualBeginsBlackStarsRise :: CardDef
theRitualBeginsBlackStarsRise = agenda "03278" "The Ritual Begins" 1 BlackStarsRise

theEntityAboveTheFloodBelow :: CardDef
theEntityAboveTheFloodBelow = agenda "03279a" "The Entity Above" 2 BlackStarsRise

theEntityAboveTheVortexAbove :: CardDef
theEntityAboveTheVortexAbove = agenda "03279b" "The Entity Above" 2 BlackStarsRise

swallowedSky :: CardDef
swallowedSky = agenda "03280" "Swallowed Sky" 3 BlackStarsRise

madnessCoils :: CardDef
madnessCoils = agenda "03317" "Madness Coils" 1 DimCarcosa

madnessDrowns :: CardDef
madnessDrowns = agenda "03318" "Madness Drowns" 2 DimCarcosa

madnessDies :: CardDef
madnessDies = agenda "03319" "Madness Dies" 3 DimCarcosa

expeditionIntoTheWild :: CardDef
expeditionIntoTheWild = agenda "04044" "Expedition into the Wild" 1 TheUntamedWilds

intruders :: CardDef
intruders = agenda "04045" "Intruders" 2 TheUntamedWilds

somethingStirs :: CardDef
somethingStirs = agenda "04055" "Something Stirs\8230" 1 TheDoomOfEztli

theTempleWarden :: CardDef
theTempleWarden = agenda "04056" "The Temple Warden" 2 TheDoomOfEztli

threeFates :: CardDef
threeFates = agenda "04114" "Three Fates" 1 ThreadsOfFate

behindTheCurtain :: CardDef
behindTheCurtain = agenda "04115" "Behind the Curtain" 2 ThreadsOfFate

hiddenEntanglements :: CardDef
hiddenEntanglements = agenda "04116" "Hidden Entanglements" 3 ThreadsOfFate

theBoundaryBroken :: CardDef
theBoundaryBroken = agenda "04162" "The Boundary, Broken" 1 TheBoundaryBeyond

theBarrierIsThin :: CardDef
theBarrierIsThin = agenda "04163" "The Barrier Is Thin" 2 TheBoundaryBeyond

timeCollapsing :: CardDef
timeCollapsing = agenda "04164" "Time Collapsing" 3 TheBoundaryBeyond

theJunglesHeart :: CardDef
theJunglesHeart = agenda "04207" "The Jungle's Heart" 1 PillarsOfJudgement

settingSun :: CardDef
settingSun = agenda "04208" "Setting Sun" 2 PillarsOfJudgement

theLonelyCaverns :: CardDef
theLonelyCaverns = agenda "04211" "The Lonely Caverns" 1 KnYan

eyesInTheDark :: CardDef
eyesInTheDark = agenda "04212" "Eyes in the Dark" 2 KnYan

cityOfTheGreatRace :: CardDef
cityOfTheGreatRace = agenda "04238" "City of the Great Race" 1 TheCityOfArchives

lostMemories :: CardDef
lostMemories = agenda "04239" "Lost Memories" 2 TheCityOfArchives

humanityFading :: CardDef
humanityFading = agenda "04240" "Humanity Fading" 3 TheCityOfArchives

theDescentBegins :: CardDef
theDescentBegins = agenda "04278" "The Descent Begins" 1 TheDepthsOfYoth

horrificDescent :: CardDef
horrificDescent = agenda "04279" "Horrific Descent" 2 TheDepthsOfYoth

endlessCaverns :: CardDef
endlessCaverns = agenda "04280" "Endless Caverns" 3 TheDepthsOfYoth

cityOfBlood :: CardDef
cityOfBlood = agenda "04281" "City of Blood" 4 TheDepthsOfYoth

furyThatShakesTheEarth :: CardDef
furyThatShakesTheEarth = agenda "04282" "Fury That Shakes the Earth" 5 TheDepthsOfYoth

theRedDepths :: CardDef
theRedDepths = agenda "04283" "The Red Depths" 6 TheDepthsOfYoth

vengeance :: CardDef
vengeance = agenda "04284" "VENGEANCE" 7 TheDepthsOfYoth

threadsOfTime :: CardDef
threadsOfTime = (agenda "04315" "Threads of Time" 1 ShatteredAeons) {cdVengeancePoints = Just 1}

pendulousThreads :: CardDef
pendulousThreads = agenda "04316" "Pendulous Threads" 2 ShatteredAeons

snappedThreads :: CardDef
snappedThreads = agenda "04317" "Snapped Threads" 3 ShatteredAeons

judgementXX :: CardDef
judgementXX = agenda "05044" "JUDGEMENT • XX" 1 DisappearanceAtTheTwilightEstate

temperanceXIV :: CardDef
temperanceXIV = agenda "05051" "TEMPERANCE • XIV" 1 TheWitchingHour

theNightHowls :: CardDef
theNightHowls = agenda "05052" "The Night Howls" 2 TheWitchingHour

justiceXI :: CardDef
justiceXI = agenda "05066" "JUSTICE • XI" 1 AtDeathsDoorstep

overTheThreshold :: CardDef
overTheThreshold = agenda "05067" "Over the Threshold" 2 AtDeathsDoorstep

theHermitIX :: CardDef
theHermitIX = agenda "05121" "THE HERMIT • IX" 1 TheSecretName

theFamiliar :: CardDef
theFamiliar = agenda "05122" "The Familiar" 2 TheSecretName

theWitchLight :: CardDef
theWitchLight = agenda "05123" "The Witch Light" 3 TheSecretName

markedForSacrifice :: CardDef
markedForSacrifice = agenda "05124" "Marked for Sacrifice" 4 TheSecretName

theHangedManXII :: CardDef
theHangedManXII = agenda "05162" "THE HANGED MAN • XII" 1 TheWagesOfSin

deathsApproach :: CardDef
deathsApproach = agenda "05163" "Death's Approach" 2 TheWagesOfSin

theHierophantV :: CardDef
theHierophantV = agenda "05198" "THE HIEROPHANT · V" 1 ForTheGreaterGood

endsAndMeans :: CardDef
endsAndMeans = agenda "05199" "Ends and Means" 2 ForTheGreaterGood

theLoversVI :: CardDef
theLoversVI = agenda "05239" "THE LOVERS · VI" 1 UnionAndDisillusion

crossroadsOfFate :: CardDef
crossroadsOfFate = agenda "05240" "Crossroads of Fate" 2 UnionAndDisillusion

theChariotVII :: CardDef
theChariotVII = agenda "05285" "THE CHARIOT · VII" 1 InTheClutchesOfChaos

wheelOfFortuneX :: CardDef
wheelOfFortuneX = agenda "05326" "WHEEL OF FORTUNE · X" 1 BeforeTheBlackThrone

itAwaits :: CardDef
itAwaits = agenda "05327" "It Awaits" 2 BeforeTheBlackThrone

theFinalCountdown :: CardDef
theFinalCountdown = agenda "05328" "The Final Countdown" 3 BeforeTheBlackThrone

returnToPredatorOrPrey :: CardDef
returnToPredatorOrPrey =
  agenda "50026" "Predator or Prey?" 1 ReturnToTheMidnightMasks

aCreatureOfTheBayou :: CardDef
aCreatureOfTheBayou = agenda "81002" "A Creature of the Bayou" 1 TheBayou

theRougarouFeeds :: CardDef
theRougarouFeeds = agenda "81003" "The Rougarou Feeds" 2 TheBayou

theCurseSpreads :: CardDef
theCurseSpreads = agenda "81004" "The Curse Spreads" 3 TheBayou

theFestivitiesBegin :: CardDef
theFestivitiesBegin =
  agenda "82002" "The Festivities Begin" 1 CarnevaleOfHorrors

theShadowOfTheEclipse :: CardDef
theShadowOfTheEclipse =
  agenda "82003" "The Shadow of the Eclipse" 2 CarnevaleOfHorrors

chaosAtTheCarnevale :: CardDef
chaosAtTheCarnevale =
  agenda "82004" "Chaos at the Carnevale" 3 CarnevaleOfHorrors
