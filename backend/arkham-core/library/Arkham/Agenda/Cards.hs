module Arkham.Agenda.Cards where

import Arkham.Prelude hiding (fold)

import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.CardType
import Arkham.Types.EncounterSet
import Arkham.Types.Name

agenda :: CardCode -> Name -> EncounterSet -> CardDef
agenda cardCode name encounterSet = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = AgendaType
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
  , cdEncounterSetQuantity = Nothing
  , cdUnique = False
  , cdDoubleSided = True
  , cdLimits = []
  , cdExceptional = False
  }

allAgendaCards :: Map CardCode CardDef
allAgendaCards = mapFromList $ map
  (toCardCode &&& id)
  [ whatsGoingOn
  , riseOfTheGhouls
  , theyreGettingOut
  , predatorOrPrey
  , timeIsRunningShort
  , theArkhamWoods
  , theRitualBegins
  , vengeanceAwaits
  , quietHalls
  , deadOfNight
  , theBeastUnleashed
  , theCloverClub
  , undergroundMuscle
  , chaosInTheCloverClub
  , restrictedAccess
  , shadowsDeepen
  , inEveryShadow
  , aTearInReality
  , theMawWidens
  , rollingBackwards
  , drawnIn
  , outOfTime
  , strangeDisappearances
  , theOldOnesHunger
  , feedTheBeast
  , rampagingCreatures
  , bidingItsTime
  , horrorsUnleashed
  , callingForthTheOldOnes
  , beckoningForPower
  , allIsOne
  , pastPresentAndFuture
  , breakingThrough
  , theEndOfAllThings
  , returnToPredatorOrPrey
  , aCreatureOfTheBayou
  , theRougarouFeeds
  , theCurseSpreads
  , theFestivitiesBegin
  , theShadowOfTheEclipse
  , chaosInTheCarnevale
  ]

whatsGoingOn :: CardDef
whatsGoingOn = agenda "01105" "What's Going On?!" TheGathering

riseOfTheGhouls :: CardDef
riseOfTheGhouls = agenda "01106" "Rise of the Ghouls" TheGathering

theyreGettingOut :: CardDef
theyreGettingOut = agenda "01107" "They're Getting Out!" TheGathering

predatorOrPrey :: CardDef
predatorOrPrey = agenda "01121" "Predator or Prey?" TheMidnightMasks

timeIsRunningShort :: CardDef
timeIsRunningShort = agenda "01122" "Time Is Running Short" TheMidnightMasks

theArkhamWoods :: CardDef
theArkhamWoods = agenda "01143" "The Arkham Woods" TheDevourerBelow

theRitualBegins :: CardDef
theRitualBegins = agenda "01144" "The Ritual Begins" TheDevourerBelow

vengeanceAwaits :: CardDef
vengeanceAwaits = agenda "01145" "Vengeance Awaits" TheDevourerBelow

quietHalls :: CardDef
quietHalls = agenda "02042" "Quiet Halls" ExtracurricularActivity

deadOfNight :: CardDef
deadOfNight = agenda "02043" "Dead of Night" ExtracurricularActivity

theBeastUnleashed :: CardDef
theBeastUnleashed =
  agenda "02044" "The Beast Unleashed" ExtracurricularActivity

theCloverClub :: CardDef
theCloverClub = agenda "02063" "The Clover Club" TheHouseAlwaysWins

undergroundMuscle :: CardDef
undergroundMuscle = agenda "02064" "Underground Muscle" TheHouseAlwaysWins

chaosInTheCloverClub :: CardDef
chaosInTheCloverClub =
  agenda "02065" "Chaos in the Clover Club" TheHouseAlwaysWins

restrictedAccess :: CardDef
restrictedAccess = agenda "02119" "Restricted Access" TheMiskatonicMuseum

shadowsDeepen :: CardDef
shadowsDeepen = agenda "02120" "Shadows Deepen" TheMiskatonicMuseum

inEveryShadow :: CardDef
inEveryShadow = agenda "02121" "In Every Shadow" TheMiskatonicMuseum

aTearInReality :: CardDef
aTearInReality = agenda "02160" "A Tear in Reality" TheEssexCountyExpress

theMawWidens :: CardDef
theMawWidens = agenda "02161" "The Maw Widens" TheEssexCountyExpress

rollingBackwards :: CardDef
rollingBackwards = agenda "02162" "Rolling Backwards" TheEssexCountyExpress

drawnIn :: CardDef
drawnIn = agenda "02163" "Drawn In" TheEssexCountyExpress

outOfTime :: CardDef
outOfTime = agenda "02164" "Out of Time" TheEssexCountyExpress

strangeDisappearances :: CardDef
strangeDisappearances = agenda "02196" "Strange Disappearances" BloodOnTheAltar

theOldOnesHunger :: CardDef
theOldOnesHunger = agenda "02197" "The Old Ones Hunger" BloodOnTheAltar

feedTheBeast :: CardDef
feedTheBeast = agenda "02198" "Feed the Beast" BloodOnTheAltar

rampagingCreatures :: CardDef
rampagingCreatures =
  agenda "02237" "Rampaging Creatures" UndimensionedAndUnseen

bidingItsTime :: CardDef
bidingItsTime = agenda "02238" "Biding Its Time" UndimensionedAndUnseen

horrorsUnleashed :: CardDef
horrorsUnleashed = agenda "02239" "Horrors Unleashed" UndimensionedAndUnseen

callingForthTheOldOnes :: CardDef
callingForthTheOldOnes =
  agenda "02275" "Calling Forth the Old Ones" WhereDoomAwaits

beckoningForPower :: CardDef
beckoningForPower = agenda "02276" "Beckoning for Power" WhereDoomAwaits

allIsOne :: CardDef
allIsOne = agenda "02312" "All is One" LostInTimeAndSpace

pastPresentAndFuture :: CardDef
pastPresentAndFuture =
  agenda "02313" "Past, Present and Future" LostInTimeAndSpace

breakingThrough :: CardDef
breakingThrough = agenda "02314" "Breaking Through" LostInTimeAndSpace

theEndOfAllThings :: CardDef
theEndOfAllThings = agenda "02315" "The End of All Things" LostInTimeAndSpace

returnToPredatorOrPrey :: CardDef
returnToPredatorOrPrey =
  agenda "50026" "Predator or Prey?" ReturnToTheMidnightMasks

aCreatureOfTheBayou :: CardDef
aCreatureOfTheBayou = agenda "81002" "A Creature of the Bayou" TheBayou

theRougarouFeeds :: CardDef
theRougarouFeeds = agenda "81003" "The Rougarou Feeds" TheBayou

theCurseSpreads :: CardDef
theCurseSpreads = agenda "81004" "The Curse Spreads" TheBayou

theFestivitiesBegin :: CardDef
theFestivitiesBegin = agenda "82002" "The Festivities Begin" CarnevaleOfHorrors

theShadowOfTheEclipse :: CardDef
theShadowOfTheEclipse =
  agenda "82003" "The Shadow of the Eclipse" CarnevaleOfHorrors

chaosInTheCarnevale :: CardDef
chaosInTheCarnevale =
  agenda "82004" "Chaos at the Carnevale" CarnevaleOfHorrors
