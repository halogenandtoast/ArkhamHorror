module Arkham.Agenda.Cards where

import Arkham.Prelude hiding (fold)

import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name

agenda :: CardCode -> Name -> Int -> EncounterSet -> CardDef
agenda cardCode name stage encounterSet = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Nothing
  , cdCost = Nothing
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
  }

allAgendaCards :: HashMap CardCode CardDef
allAgendaCards = mapFromList $ map
  (toCardCode &&& id)
  [ aCreatureOfTheBayou
  , aTearInReality
  , allIsOne
  , beckoningForPower
  , bidingItsTime
  , breakingThrough
  , callingForthTheOldOnes
  , chaosAtTheCarnevale
  , chaosInTheCloverClub
  , deadOfNight
  , drawnIn
  , empireOfTheDead
  , empireOfTheUndead
  , encore
  , expeditionIntoTheWild
  , fashionablyLate
  , feedTheBeast
  , hisDomain
  , horrorsUnleashed
  , intruders
  , inEveryShadow
  , letTheStormRageTheFloodBelow
  , letTheStormRageTheVortexAbove
  , lockedInside
  , madnessCoils
  , madnessDies
  , madnessDrowns
  , outOfTime
  , pastPresentAndFuture
  , predatorOrPrey
  , quietHalls
  , rampagingCreatures
  , ransackingTheManor
  , restrictedAccess
  , returnToPredatorOrPrey
  , riseOfTheGhouls
  , rollingBackwards
  , secretsBetterLeftHidden
  , shadowsDeepen
  , somethingStirs
  , strangeDisappearances
  , swallowedSky
  , theArkhamWoods
  , theBeastUnleashed
  , theCityFloods
  , theCloverClub
  , theCurseSpreads
  , theEndOfAllThings
  , theEntityAboveTheFloodBelow
  , theEntityAboveTheVortexAbove
  , theFestivitiesBegin
  , theFirstNight
  , theMawWidens
  , theOldOnesHunger
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
  , timeIsRunningShort
  , torturousDescent
  , undergroundMuscle
  , vengeanceAwaits
  , whatsGoingOn
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
somethingStirs = agenda "04055" "Something Stirs..." 1 TheDoomOfEztli

theTempleWarden :: CardDef
theTempleWarden = agenda "04056" "The Temple Warden" 2 TheDoomOfEztli

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
