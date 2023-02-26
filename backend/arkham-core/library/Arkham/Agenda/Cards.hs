module Arkham.Agenda.Cards where

import Arkham.Prelude hiding (fold)

import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name

agenda :: CardCode -> Name -> Int -> EncounterSet -> CardDef 'AgendaType
agenda cardCode name stage encounterSet = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdAdditionalCost = Nothing
  , cdLevel = 0
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
  }

allAgendaCards :: HashMap CardCode (CardDef 'AgendaType)
allAgendaCards = mapFrom
  toCardCode
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
  , deadOfNight
  , drawnIn
  , empireOfTheDead
  , empireOfTheUndead
  , encore
  , endlessCaverns
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
  , judgementXX
  , letTheStormRageTheFloodBelow
  , letTheStormRageTheVortexAbove
  , lockedInside
  , lostMemories
  , madnessCoils
  , madnessDies
  , madnessDrowns
  , outOfTime
  , pastPresentAndFuture
  , pendolousThreads
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
  , theArkhamWoods
  , theBarrierIsThin
  , theBeastUnleashed
  , theBoundaryBroken
  , theCityFloods
  , theCloverClub
  , theCurseSpreads
  , theDescentBegins
  , theEndOfAllThings
  , theEntityAboveTheFloodBelow
  , theEntityAboveTheVortexAbove
  , theFestivitiesBegin
  , theFirstNight
  , theJunglesHeart
  , theLonelyCaverns
  , theMawWidens
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
  ]

whatsGoingOn :: CardDef 'AgendaType
whatsGoingOn = agenda "01105" "What's Going On?!" 1 TheGathering

riseOfTheGhouls :: CardDef 'AgendaType
riseOfTheGhouls = agenda "01106" "Rise of the Ghouls" 2 TheGathering

theyreGettingOut :: CardDef 'AgendaType
theyreGettingOut = agenda "01107" "They're Getting Out!" 3 TheGathering

predatorOrPrey :: CardDef 'AgendaType
predatorOrPrey = agenda "01121" "Predator or Prey?" 1 TheMidnightMasks

timeIsRunningShort :: CardDef 'AgendaType
timeIsRunningShort = agenda "01122" "Time Is Running Short" 2 TheMidnightMasks

theArkhamWoods :: CardDef 'AgendaType
theArkhamWoods = agenda "01143" "The Arkham Woods" 1 TheDevourerBelow

theRitualBegins :: CardDef 'AgendaType
theRitualBegins = agenda "01144" "The Ritual Begins" 2 TheDevourerBelow

vengeanceAwaits :: CardDef 'AgendaType
vengeanceAwaits = agenda "01145" "Vengeance Awaits" 3 TheDevourerBelow

quietHalls :: CardDef 'AgendaType
quietHalls = agenda "02042" "Quiet Halls" 1 ExtracurricularActivity

deadOfNight :: CardDef 'AgendaType
deadOfNight = agenda "02043" "Dead of Night" 2 ExtracurricularActivity

theBeastUnleashed :: CardDef 'AgendaType
theBeastUnleashed =
  agenda "02044" "The Beast Unleashed" 3 ExtracurricularActivity

theCloverClub :: CardDef 'AgendaType
theCloverClub = agenda "02063" "The Clover Club" 1 TheHouseAlwaysWins

undergroundMuscle :: CardDef 'AgendaType
undergroundMuscle = agenda "02064" "Underground Muscle" 2 TheHouseAlwaysWins

chaosInTheCloverClub :: CardDef 'AgendaType
chaosInTheCloverClub =
  agenda "02065" "Chaos in the Clover Club" 3 TheHouseAlwaysWins

restrictedAccess :: CardDef 'AgendaType
restrictedAccess = agenda "02119" "Restricted Access" 1 TheMiskatonicMuseum

shadowsDeepen :: CardDef 'AgendaType
shadowsDeepen = agenda "02120" "Shadows Deepen" 2 TheMiskatonicMuseum

inEveryShadow :: CardDef 'AgendaType
inEveryShadow = agenda "02121" "In Every Shadow" 3 TheMiskatonicMuseum

aTearInReality :: CardDef 'AgendaType
aTearInReality = agenda "02160" "A Tear in Reality" 1 TheEssexCountyExpress

theMawWidens :: CardDef 'AgendaType
theMawWidens = agenda "02161" "The Maw Widens" 2 TheEssexCountyExpress

rollingBackwards :: CardDef 'AgendaType
rollingBackwards = agenda "02162" "Rolling Backwards" 3 TheEssexCountyExpress

drawnIn :: CardDef 'AgendaType
drawnIn = agenda "02163" "Drawn In" 4 TheEssexCountyExpress

outOfTime :: CardDef 'AgendaType
outOfTime = agenda "02164" "Out of Time" 5 TheEssexCountyExpress

strangeDisappearances :: CardDef 'AgendaType
strangeDisappearances =
  agenda "02196" "Strange Disappearances" 1 BloodOnTheAltar

theOldOnesHunger :: CardDef 'AgendaType
theOldOnesHunger = agenda "02197" "The Old Ones Hunger" 2 BloodOnTheAltar

feedTheBeast :: CardDef 'AgendaType
feedTheBeast = agenda "02198" "Feed the Beast" 3 BloodOnTheAltar

rampagingCreatures :: CardDef 'AgendaType
rampagingCreatures =
  agenda "02237" "Rampaging Creatures" 1 UndimensionedAndUnseen

bidingItsTime :: CardDef 'AgendaType
bidingItsTime = agenda "02238" "Biding Its Time" 2 UndimensionedAndUnseen

horrorsUnleashed :: CardDef 'AgendaType
horrorsUnleashed = agenda "02239" "Horrors Unleashed" 3 UndimensionedAndUnseen

callingForthTheOldOnes :: CardDef 'AgendaType
callingForthTheOldOnes =
  agenda "02275" "Calling Forth the Old Ones" 1 WhereDoomAwaits

beckoningForPower :: CardDef 'AgendaType
beckoningForPower = agenda "02276" "Beckoning for Power" 2 WhereDoomAwaits

allIsOne :: CardDef 'AgendaType
allIsOne = agenda "02312" "All is One" 1 LostInTimeAndSpace

pastPresentAndFuture :: CardDef 'AgendaType
pastPresentAndFuture =
  agenda "02313" "Past, Present and Future" 2 LostInTimeAndSpace

breakingThrough :: CardDef 'AgendaType
breakingThrough = agenda "02314" "Breaking Through" 3 LostInTimeAndSpace

theEndOfAllThings :: CardDef 'AgendaType
theEndOfAllThings = agenda "02315" "The End of All Things" 4 LostInTimeAndSpace

theThirdAct :: CardDef 'AgendaType
theThirdAct = agenda "03044" "The Third Act" 1 CurtainCall

encore :: CardDef 'AgendaType
encore = agenda "03045" "Encore" 2 CurtainCall

fashionablyLate :: CardDef 'AgendaType
fashionablyLate = agenda "03062" "Fashionably Late" 1 TheLastKing

theTerrifyingTruth :: CardDef 'AgendaType
theTerrifyingTruth = agenda "03063" "The Terrifying Truth" 2 TheLastKing

theTruthIsHidden :: CardDef 'AgendaType
theTruthIsHidden = agenda "03121" "The Truth is Hidden" 1 EchoesOfThePast

ransackingTheManor :: CardDef 'AgendaType
ransackingTheManor = agenda "03122" "Ransacking the Manor" 2 EchoesOfThePast

secretsBetterLeftHidden :: CardDef 'AgendaType
secretsBetterLeftHidden =
  agenda "03123" "Secrets Better Left Hidden" 3 EchoesOfThePast

lockedInside :: CardDef 'AgendaType
lockedInside = agenda "03160" "Locked Inside" 1 TheUnspeakableOath

torturousDescent :: CardDef 'AgendaType
torturousDescent = agenda "03161" "Torturous Descent" 2 TheUnspeakableOath

hisDomain :: CardDef 'AgendaType
hisDomain = agenda "03162" "His Domain" 3 TheUnspeakableOath

theFirstNight :: CardDef 'AgendaType
theFirstNight = agenda "03201" "The First Night" 1 APhantomOfTruth

theSecondNight :: CardDef 'AgendaType
theSecondNight = agenda "03202" "The Second Night" 2 APhantomOfTruth

theThirdNight :: CardDef 'AgendaType
theThirdNight = agenda "03203" "The Third Night" 3 APhantomOfTruth

empireOfTheDead :: CardDef 'AgendaType
empireOfTheDead = agenda "03241" "Empire of the Dead" 1 ThePallidMask

empireOfTheUndead :: CardDef 'AgendaType
empireOfTheUndead = agenda "03242" "Empire of the Undead" 2 ThePallidMask

theTideRises :: CardDef 'AgendaType
theTideRises = agenda "03275" "The Tide Rises" 1 BlackStarsRise

letTheStormRageTheFloodBelow :: CardDef 'AgendaType
letTheStormRageTheFloodBelow = agenda "03276a" "Let The Storm Rage" 2 BlackStarsRise

letTheStormRageTheVortexAbove :: CardDef 'AgendaType
letTheStormRageTheVortexAbove = agenda "03276b" "Let The Storm Rage" 2 BlackStarsRise

theCityFloods :: CardDef 'AgendaType
theCityFloods = agenda "03277" "The City Floods" 3 BlackStarsRise

theRitualBeginsBlackStarsRise :: CardDef 'AgendaType
theRitualBeginsBlackStarsRise = agenda "03278" "The Ritual Begins" 1 BlackStarsRise

theEntityAboveTheFloodBelow :: CardDef 'AgendaType
theEntityAboveTheFloodBelow = agenda "03279a" "The Entity Above" 2 BlackStarsRise

theEntityAboveTheVortexAbove :: CardDef 'AgendaType
theEntityAboveTheVortexAbove = agenda "03279b" "The Entity Above" 2 BlackStarsRise

swallowedSky :: CardDef 'AgendaType
swallowedSky = agenda "03280" "Swallowed Sky" 3 BlackStarsRise

madnessCoils :: CardDef 'AgendaType
madnessCoils = agenda "03317" "Madness Coils" 1 DimCarcosa

madnessDrowns :: CardDef 'AgendaType
madnessDrowns = agenda "03318" "Madness Drowns" 2 DimCarcosa

madnessDies :: CardDef 'AgendaType
madnessDies = agenda "03319" "Madness Dies" 3 DimCarcosa

expeditionIntoTheWild :: CardDef 'AgendaType
expeditionIntoTheWild = agenda "04044" "Expedition into the Wild" 1 TheUntamedWilds

intruders :: CardDef 'AgendaType
intruders = agenda "04045" "Intruders" 2 TheUntamedWilds

somethingStirs :: CardDef 'AgendaType
somethingStirs = agenda "04055" "Something Stirs\8230" 1 TheDoomOfEztli

theTempleWarden :: CardDef 'AgendaType
theTempleWarden = agenda "04056" "The Temple Warden" 2 TheDoomOfEztli

threeFates :: CardDef 'AgendaType
threeFates = agenda "04114" "Three Fates" 1 ThreadsOfFate

behindTheCurtain :: CardDef 'AgendaType
behindTheCurtain = agenda "04115" "Behind the Curtain" 2 ThreadsOfFate

hiddenEntanglements :: CardDef 'AgendaType
hiddenEntanglements = agenda "04116" "Hidden Entanglements" 3 ThreadsOfFate

theBoundaryBroken :: CardDef 'AgendaType
theBoundaryBroken = agenda "04162" "The Boundary, Broken" 1 TheBoundaryBeyond

theBarrierIsThin :: CardDef 'AgendaType
theBarrierIsThin = agenda "04163" "The Barrier is Thin" 2 TheBoundaryBeyond

timeCollapsing :: CardDef 'AgendaType
timeCollapsing = agenda "04164" "Time Collapsing" 3 TheBoundaryBeyond

theJunglesHeart :: CardDef 'AgendaType
theJunglesHeart = agenda "04207" "The Jungle's Heart" 1 PillarsOfJudgement

settingSun :: CardDef 'AgendaType
settingSun = agenda "04208" "Setting Sun" 2 PillarsOfJudgement

theLonelyCaverns :: CardDef 'AgendaType
theLonelyCaverns = agenda "04211" "The Lonely Caverns" 1 KnYan

eyesInTheDark :: CardDef 'AgendaType
eyesInTheDark = agenda "04212" "Eyes in the Dark" 2 KnYan

cityOfTheGreatRace :: CardDef 'AgendaType
cityOfTheGreatRace = agenda "04238" "City of the Great Race" 1 TheCityOfArchives

lostMemories :: CardDef 'AgendaType
lostMemories = agenda "04239" "Lost Memories" 2 TheCityOfArchives

humanityFading :: CardDef 'AgendaType
humanityFading = agenda "04240" "Humanity Fading" 3 TheCityOfArchives

theDescentBegins :: CardDef 'AgendaType
theDescentBegins = agenda "04278" "The Descent Begins" 1 TheDepthsOfYoth

horrificDescent :: CardDef 'AgendaType
horrificDescent = agenda "04279" "Horrific Descent" 2 TheDepthsOfYoth

endlessCaverns :: CardDef 'AgendaType
endlessCaverns = agenda "04280" "Endless Caverns" 3 TheDepthsOfYoth

cityOfBlood :: CardDef 'AgendaType
cityOfBlood = agenda "04281" "City of Blood" 4 TheDepthsOfYoth

furyThatShakesTheEarth :: CardDef 'AgendaType
furyThatShakesTheEarth = agenda "04282" "Fury That Shakes the Earth" 5 TheDepthsOfYoth

theRedDepths :: CardDef 'AgendaType
theRedDepths = agenda "04283" "The Red Depths" 6 TheDepthsOfYoth

vengeance :: CardDef 'AgendaType
vengeance = agenda "04284" "VENGEANCE" 7 TheDepthsOfYoth

threadsOfTime :: CardDef 'AgendaType
threadsOfTime = (agenda "04315" "Threads of Time" 1 ShatteredAeons) { cdVengeancePoints = Just 1 }

pendolousThreads :: CardDef 'AgendaType
pendolousThreads = agenda "04316" "Pendolous Threads" 2 ShatteredAeons

snappedThreads :: CardDef 'AgendaType
snappedThreads = agenda "04317" "Snapped Threads" 3 ShatteredAeons

judgementXX :: CardDef 'AgendaType
judgementXX = agenda "05044" "JUDGEMENT • XX" 1 DisappearanceAtTheTwilightEstate

returnToPredatorOrPrey :: CardDef 'AgendaType
returnToPredatorOrPrey =
  agenda "50026" "Predator or Prey?" 1 ReturnToTheMidnightMasks

aCreatureOfTheBayou :: CardDef 'AgendaType
aCreatureOfTheBayou = agenda "81002" "A Creature of the Bayou" 1 TheBayou

theRougarouFeeds :: CardDef 'AgendaType
theRougarouFeeds = agenda "81003" "The Rougarou Feeds" 2 TheBayou

theCurseSpreads :: CardDef 'AgendaType
theCurseSpreads = agenda "81004" "The Curse Spreads" 3 TheBayou

theFestivitiesBegin :: CardDef 'AgendaType
theFestivitiesBegin =
  agenda "82002" "The Festivities Begin" 1 CarnevaleOfHorrors

theShadowOfTheEclipse :: CardDef 'AgendaType
theShadowOfTheEclipse =
  agenda "82003" "The Shadow of the Eclipse" 2 CarnevaleOfHorrors

chaosAtTheCarnevale :: CardDef 'AgendaType
chaosAtTheCarnevale =
  agenda "82004" "Chaos at the Carnevale" 3 CarnevaleOfHorrors
