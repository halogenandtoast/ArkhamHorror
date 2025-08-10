module Arkham.Agenda.Cards where

import Arkham.Prelude hiding (fold)

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name

agenda :: CardCode -> Name -> Int -> EncounterSet -> CardDef
agenda cardCode name stage encounterSet =
  (emptyCardDef cardCode name AgendaType)
    { cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Nothing
    , cdDoubleSided = True
    , cdStage = Just stage
    , cdLevel = Nothing
    }

allAgendaCards :: Map CardCode CardDef
allAgendaCards =
  mapFromList
    $ map
      (toCardCode &&& id)
      [ aCreatureOfTheBayou
      , agentsOfTheOuterGods
      , aHarshWindBlows
      , aKillerParty
      , allIsOne
      , aSinisterRealm
      , aTearInReality
      , aTearInRealityV2
      , aTrailOfTwists
      , awakening
      , barricadedStreets
      , beckoningForPower
      , behindTheCurtain
      , besetByMonsters
      , betterNeverThanLate
      , bidingItsTime
      , breakingThrough
      , breakingThroughV2
      , callingForthTheOldOnes
      , callOfMadness
      , celestialAlignment
      , chaosAtTheCarnevale
      , chaosIncarnate
      , chaosInTheCloverClub
      , cityOfBlood
      , cityOfTheGreatRace
      , coldWelcome
      , crossroadsOfFate
      , deadOfNight
      , deathsApproach
      , decrepitDecay
      , doomFromBelow
      , drawnIn
      , empireOfTheDead
      , empireOfTheUndead
      , encore
      , endlessCaverns
      , endsAndMeans
      , etherealTangleV1
      , etherealTangleV2
      , etherealTangleV3
      , expeditionIntoTheWild
      , eyesInTheDark
      , fashionablyLate
      , feedTheBeast
      , floodedStreets
      , fogOnTheBay
      , forbiddenPeaks
      , franticPursuit
      , furyThatShakesTheEarth
      , growingSuspicion
      , hallsOfStMarys
      , hiddenEntanglements
      , hisDomain
      , horrificDescent
      , horrorsUnleashed
      , hospitalOfHorrors
      , hotPursuit
      , humanityFading
      , icyDepths
      , inEveryShadow
      , intoTheWhite
      , intruders
      , itAwaits
      , journeyAcrossTheDreamlands
      , journeyThroughTheGates
      , judgementXX
      , justiceXI
      , letTheStormRageTheFloodBelow
      , letTheStormRageTheVortexAbove
      , lockedInside
      , lostMemories
      , lurkingHorrors
      , madnessAndDeath
      , madnessCoils
      , madnessDies
      , madnessDrowns
      , manifestationsOfEvil
      , markedForSacrifice
      , maskedRevelers
      , outOfTime
      , overTheThreshold
      , pastPresentAndFuture
      , pendulousThreads
      , predatorOrPrey
      , quietHalls
      , rageOfTheDeep
      , rampagingCreatures
      , ransackingTheManor
      , realitiesInterwoven
      , relentlessTide
      , restrictedAccess
      , returnToPredatorOrPrey
      , riseOfTheGhouls
      , rollingBackwards
      , runningOutOfTime
      , sacrificeForTheDeep
      , secretsBetterLeftHidden
      , secretsOfTheSeaV1
      , secretsOfTheSeaV2
      , settingSun
      , shadowsDeepen
      , silentStirring
      , snappedThreads
      , somethingStirs
      , specialInvestigation
      , strangeDisappearances
      , swallowedSky
      , temperanceXIV
      , terrorAtFalconPoint
      , terrorDescends
      , thatWhichHasNoName
      , theAlarmIsRaised
      , theArkhamWoods
      , theBarrierIsThin
      , theBeastUnleashed
      , theBeatingHeart
      , theBoundaryBroken
      , theBridgeOfWebs
      , theChariotVII
      , theChaseIsOnV1
      , theChaseIsOnV2
      , theChillOfNight
      , theCityFloods
      , theCloverClub
      , theCurseSpreads
      , theDeadWithNoName
      , theDescentBegins
      , theDevilOfTheDepths
      , theEndOfAllThings
      , theEntityAboveTheFloodBelow
      , theEntityAboveTheVortexAbove
      , theEyeOfChaos
      , theFamiliar
      , theFestivitiesBegin
      , theFinalCountdown
      , theFirstNight
      , theFlood
      , theHangedManXII
      , theHermitIX
      , theHierophantV
      , theHouseWithNoName
      , theInfestationSpreads
      , theInitiationV1
      , theInitiationV2
      , theJunglesHeart
      , theLonelyCaverns
      , theLoversVI
      , theMawWidens
      , theMiasmaBeckons
      , theMurder
      , theNightHowls
      , theOldOnesHunger
      , theRedDepths
      , theRitualAdvances
      , theRitualBegins
      , theRitualBeginsBlackStarsRise
      , theRougarouFeeds
      , theSealWeakens
      , theSecondNight
      , theShadowOfTheEclipse
      , theShapeOfChaos
      , theTempleWarden
      , theTerrifyingTruth
      , theThingWithNoName
      , theThirdAct
      , theThirdNight
      , theTideRises
      , theTideRisesALightInTheFog
      , theTrueCulpritV1
      , theTrueCulpritV10
      , theTrueCulpritV2
      , theTrueCulpritV3
      , theTrueCulpritV4
      , theTrueCulpritV5
      , theTrueCulpritV6
      , theTrueCulpritV7
      , theTrueCulpritV8
      , theTrueCulpritV9
      , theTruthIsHidden
      , theWaterRises
      , theWitchLight
      , theyAreUponYou
      , theyreGettingOut
      , threadsOfTime
      , threeFates
      , timeCollapsing
      , timeIsRunningShort
      , torturousDescent
      , unchangingAsTheSea
      , undergroundMuscle
      , underTheSurface
      , unexpectedGuests
      , vengeance
      , vengeanceAwaits
      , whatLurksBelowV1
      , whatLurksBelowV2
      , whatsGoingOn
      , wheelOfFortuneX
      , whereTheresSmoke
      ]

whatsGoingOn :: CardDef
whatsGoingOn = agenda "01105" "What's Going On?!" 1 TheGathering

riseOfTheGhouls :: CardDef
riseOfTheGhouls = agenda "01106" "Rise of the Ghouls" 2 TheGathering

theyreGettingOut :: CardDef
theyreGettingOut = agenda "01107" "They're Getting Out!" 3 TheGathering

predatorOrPrey :: CardDef
predatorOrPrey = agenda "01121a" "Predator or Prey?" 1 TheMidnightMasks

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
letTheStormRageTheFloodBelow = (agenda "03276a" "Let The Storm Rage" 2 BlackStarsRise) {cdOtherSide = Just "03276ab"}

letTheStormRageTheVortexAbove :: CardDef
letTheStormRageTheVortexAbove = agenda "03276b" "Let The Storm Rage" 2 BlackStarsRise

theCityFloods :: CardDef
theCityFloods = agenda "03277" "The City Floods" 3 BlackStarsRise

theRitualBeginsBlackStarsRise :: CardDef
theRitualBeginsBlackStarsRise = agenda "03278" "The Ritual Begins" 1 BlackStarsRise

theEntityAboveTheFloodBelow :: CardDef
theEntityAboveTheFloodBelow = (agenda "03279a" "The Entity Above" 2 BlackStarsRise) {cdOtherSide = Just "03279ab"}

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
theHangedManXII = agenda "05162" "THE HANGED MAN · XII" 1 TheWagesOfSin

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

journeyThroughTheGates :: CardDef
journeyThroughTheGates = agenda "06040" "Journey through the Gates" 1 BeyondTheGatesOfSleep

hallsOfStMarys :: CardDef
hallsOfStMarys = agenda "06064" "Halls of St. Mary's" 1 WakingNightmare

theInfestationSpreads :: CardDef
theInfestationSpreads = agenda "06065" "The Infestation Spreads" 2 WakingNightmare

hospitalOfHorrors :: CardDef
hospitalOfHorrors = agenda "06066" "Hospital of Horrors" 3 WakingNightmare

journeyAcrossTheDreamlands :: CardDef
journeyAcrossTheDreamlands = agenda "06120" "Journey Across the Dreamlands" 1 TheSearchForKadath

agentsOfTheOuterGods :: CardDef
agentsOfTheOuterGods = agenda "06121" "Agents of the Outer Gods" 2 TheSearchForKadath

theHouseWithNoName :: CardDef
theHouseWithNoName = agenda "06169a" "The House with No Name" 1 AThousandShapesOfHorror

theThingWithNoName :: CardDef
theThingWithNoName = agenda "06170" "The Thing with No Name" 2 AThousandShapesOfHorror

theDeadWithNoName :: CardDef
theDeadWithNoName = agenda "06171" "The Dead with No Name" 3 AThousandShapesOfHorror

silentStirring :: CardDef
silentStirring = agenda "06207" "Silent Stirring" 1 DarkSideOfTheMoon

theAlarmIsRaised :: CardDef
theAlarmIsRaised = agenda "06208" "The Alarm Is Raised" 2 DarkSideOfTheMoon

theyAreUponYou :: CardDef
theyAreUponYou = agenda "06209" "They Are Upon You!" 3 DarkSideOfTheMoon

aSinisterRealm :: CardDef
aSinisterRealm = agenda "06248" "A Sinister Realm" 1 PointOfNoReturn

besetByMonsters :: CardDef
besetByMonsters = agenda "06249" "Beset by Monsters" 2 PointOfNoReturn

theEyeOfChaos :: CardDef
theEyeOfChaos = agenda "06287" "The Eye of Chaos" 1 WhereTheGodsDwell

theShapeOfChaos :: CardDef
theShapeOfChaos = agenda "06288" "The Shape of Chaos" 2 WhereTheGodsDwell

chaosIncarnate :: CardDef
chaosIncarnate = agenda "06289" "Chaos Incarnate" 3 WhereTheGodsDwell

theBridgeOfWebs :: CardDef
theBridgeOfWebs = agenda "06334" "The Bridge of Webs" 1 WeaverOfTheCosmos

aTrailOfTwists :: CardDef
aTrailOfTwists = agenda "06335" "A Trail of Twists" 2 WeaverOfTheCosmos

realitiesInterwoven :: CardDef
realitiesInterwoven = agenda "06336" "Realities Interwoven" 3 WeaverOfTheCosmos

awakening :: CardDef
awakening = agenda "07042" "Awakening" 1 ThePitOfDespair

theWaterRises :: CardDef
theWaterRises = agenda "07043" "The Water Rises" 2 ThePitOfDespair

sacrificeForTheDeep :: CardDef
sacrificeForTheDeep = agenda "07044" "Sacrifice for the Deep" 3 ThePitOfDespair

decrepitDecay :: CardDef
decrepitDecay = agenda "07057" "Decrepit Decay" 1 TheVanishingOfElinaHarper

growingSuspicion :: CardDef
growingSuspicion = agenda "07058" "Growing Suspicion" 2 TheVanishingOfElinaHarper

franticPursuit :: CardDef
franticPursuit = agenda "07059" "Frantic Pursuit" 3 TheVanishingOfElinaHarper

barricadedStreets :: CardDef
barricadedStreets = agenda "07124" "Barricaded Streets" 1 InTooDeep

relentlessTide :: CardDef
relentlessTide = agenda "07125" "Relentless Tide" 2 InTooDeep

floodedStreets :: CardDef
floodedStreets = agenda "07126" "Flooded Streets" 3 InTooDeep

rageOfTheDeep :: CardDef
rageOfTheDeep = agenda "07127" "Rage of the Deep" 4 InTooDeep

secretsOfTheSeaV1 :: CardDef
secretsOfTheSeaV1 = agenda "07164" "Secrets of the Sea (v. I)" 1 DevilReef

secretsOfTheSeaV2 :: CardDef
secretsOfTheSeaV2 = agenda "07165" "Secrets of the Sea (v. II)" 1 DevilReef

theDevilOfTheDepths :: CardDef
theDevilOfTheDepths = agenda "07166" "The Devil of the Depths" 2 DevilReef

theChaseIsOnV1 :: CardDef
theChaseIsOnV1 = agenda "07199" "The Chase is On! (v. I)" 1 HorrorInHighGear

theChaseIsOnV2 :: CardDef
theChaseIsOnV2 = agenda "07200" "The Chase is On! (v. II)" 1 HorrorInHighGear

hotPursuit :: CardDef
hotPursuit = agenda "07201" "Hot Pursuit" 2 HorrorInHighGear

fogOnTheBay :: CardDef
fogOnTheBay = agenda "07232" "Fog on the Bay" 1 ALightInTheFog

unchangingAsTheSea :: CardDef
unchangingAsTheSea = agenda "07233" "Unchanging as the Sea" 2 ALightInTheFog

theTideRisesALightInTheFog :: CardDef
theTideRisesALightInTheFog = agenda "07234" "The Tide Rises" 3 ALightInTheFog

terrorAtFalconPoint :: CardDef
terrorAtFalconPoint = agenda "07235" "Terror at Falcon Point" 4 ALightInTheFog

theInitiationV1 :: CardDef
theInitiationV1 = agenda "07275" "The Initiation (v. I)" 1 TheLairOfDagon

theInitiationV2 :: CardDef
theInitiationV2 = agenda "07276" "The Initiation (v. II)" 1 TheLairOfDagon

whatLurksBelowV1 :: CardDef
whatLurksBelowV1 = agenda "07277" "What Lurks Below (v. I)" 2 TheLairOfDagon

whatLurksBelowV2 :: CardDef
whatLurksBelowV2 = agenda "07278" "What Lurks Below (v. II)" 2 TheLairOfDagon

theRitualAdvances :: CardDef
theRitualAdvances = agenda "07279" "The Ritual Advances" 3 TheLairOfDagon

underTheSurface :: CardDef
underTheSurface = agenda "07312" "Under the Surface" 1 IntoTheMaelstrom

celestialAlignment :: CardDef
celestialAlignment = agenda "07313" "Celestial Alignment" 2 IntoTheMaelstrom

theFlood :: CardDef
theFlood = agenda "07314" "The Flood" 3 IntoTheMaelstrom

coldWelcome :: CardDef
coldWelcome = agenda "08518" "Cold Welcome" 1 TheCrash

intoTheWhite :: CardDef
intoTheWhite = agenda "08519" "Into the White" 2 TheCrash

runningOutOfTime :: CardDef
runningOutOfTime = agenda "08520" "Running Out of Time" 3 TheCrash

aHarshWindBlows :: CardDef
aHarshWindBlows = agenda "08523" "A Harsh Window Blows" 4 LostInTheNight

theChillOfNight :: CardDef
theChillOfNight = agenda "08524" "The Chill of Night" 5 LostInTheNight

madnessAndDeath :: CardDef
madnessAndDeath = agenda "08525" "Madness and Death" 6 LostInTheNight

manifestationsOfEvil :: CardDef
manifestationsOfEvil = agenda "08544" "Manifestations of Evil" 7 SeepingNightmares

icyDepths :: CardDef
icyDepths = agenda "08545" "Icy Depths" 8 SeepingNightmares

etherealTangleV1 :: CardDef
etherealTangleV1 = agenda "08550" "Ethereal Tangle (v. I)" 1 FatalMirage

etherealTangleV2 :: CardDef
etherealTangleV2 = agenda "08551" "Ethereal Tangle (v. II)" 2 FatalMirage

etherealTangleV3 :: CardDef
etherealTangleV3 = agenda "08552" "Ethereal Tangle (v. III)" 3 FatalMirage

forbiddenPeaks :: CardDef
forbiddenPeaks = agenda "08597" "Forbidden Peaks" 1 ToTheForbiddenPeaks

terrorDescends :: CardDef
terrorDescends = agenda "08598" "Terror Descends" 2 ToTheForbiddenPeaks

lurkingHorrors :: CardDef
lurkingHorrors = agenda "08622" "Lurking Horrors" 1 CityOfTheElderThings

doomFromBelow :: CardDef
doomFromBelow = agenda "08623" "Doom From Below" 2 CityOfTheElderThings

theBeatingHeart :: CardDef
theBeatingHeart = agenda "08659" "The Beating Heart" 1 TheGreatSeal

theMiasmaBeckons :: CardDef
theMiasmaBeckons = agenda "08660" "The Miasma Beckons" 2 TheGreatSeal

callOfMadness :: CardDef
callOfMadness = agenda "08661" "Call of Madness" 3 TheGreatSeal

theSealWeakens :: CardDef
theSealWeakens = agenda "08671" "The Seal Weakens" 4 StirringInTheDeep

thatWhichHasNoName :: CardDef
thatWhichHasNoName = agenda "08672" "That Which Has No Name" 5 StirringInTheDeep

returnToPredatorOrPrey :: CardDef
returnToPredatorOrPrey =
  agenda "50026a" "Predator or Prey?" 1 ReturnToTheMidnightMasks

whereTheresSmoke :: CardDef
whereTheresSmoke = agenda "51026" "Where There's Smoke" 0 ReturnToTheEssexCountyExpress

aTearInRealityV2 :: CardDef
aTearInRealityV2 = agenda "51027" "A Tear in Reality (v. II)" 1 ReturnToTheEssexCountyExpress

breakingThroughV2 :: CardDef
breakingThroughV2 = agenda "51054" "Breaking Through (v. II)" 3 ReturnToLostInTimeAndSpace

betterNeverThanLate :: CardDef
betterNeverThanLate = agenda "52022" "Better Never Than Late" 1 ReturnToTheLastKing

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

theMurder :: CardDef
theMurder = agenda "84002" "The Murder" 1 MurderAtTheExcelsiorHotel

specialInvestigation :: CardDef
specialInvestigation = agenda "84003" "Special Investigation" 2 MurderAtTheExcelsiorHotel

theTrueCulpritV1 :: CardDef
theTrueCulpritV1 = agenda "84043" "The True Culprit (v. I)" 3 MurderAtTheExcelsiorHotel

theTrueCulpritV2 :: CardDef
theTrueCulpritV2 = agenda "84044" "The True Culprit (v. II)" 3 MurderAtTheExcelsiorHotel

theTrueCulpritV3 :: CardDef
theTrueCulpritV3 = agenda "84045" "The True Culprit (v. III)" 3 MurderAtTheExcelsiorHotel

theTrueCulpritV4 :: CardDef
theTrueCulpritV4 = agenda "84046" "The True Culprit (v. IV)" 3 MurderAtTheExcelsiorHotel

theTrueCulpritV5 :: CardDef
theTrueCulpritV5 = agenda "84047" "The True Culprit (v. V)" 3 MurderAtTheExcelsiorHotel

theTrueCulpritV6 :: CardDef
theTrueCulpritV6 = agenda "84048" "The True Culprit (v. VI)" 3 MurderAtTheExcelsiorHotel

theTrueCulpritV7 :: CardDef
theTrueCulpritV7 = agenda "84049" "The True Culprit (v. VII)" 3 MurderAtTheExcelsiorHotel

theTrueCulpritV8 :: CardDef
theTrueCulpritV8 = agenda "84050" "The True Culprit (v. VIII)" 3 MurderAtTheExcelsiorHotel

theTrueCulpritV9 :: CardDef
theTrueCulpritV9 = agenda "84051" "The True Culprit (v. IX)" 3 MurderAtTheExcelsiorHotel

theTrueCulpritV10 :: CardDef
theTrueCulpritV10 = agenda "84052" "The True Culprit (v. X)" 3 MurderAtTheExcelsiorHotel

maskedRevelers :: CardDef
maskedRevelers = agenda "71002" "Masked Revelers" 1 TheMidwinterGala

unexpectedGuests :: CardDef
unexpectedGuests = agenda "71003" "Unexpected Guests" 2 TheMidwinterGala

aKillerParty :: CardDef
aKillerParty = agenda "71004" "A Killer Party" 3 TheMidwinterGala
