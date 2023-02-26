module Arkham.Act.Cards where

import Arkham.Prelude hiding ( fold )

import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name

act :: CardCode -> Name -> Int -> EncounterSet -> CardDef 'ActType
act cardCode name stage encounterSet = CardDef
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

allActCards :: HashMap CardCode (CardDef 'ActType)
allActCards = mapFrom
  toCardCode
  [ afterHours
  , alejandrosPlight
  , alejandrosPrison
  , allIn
  , arkhamAsylum
  , ascendingTheHillV1
  , ascendingTheHillV2
  , ascendingTheHillV3
  , atTheExhibitTheBrotherhoodsPlot
  , atTheExhibitTheRelicsLocation
  , atTheStationInShadowedTalons
  , atTheStationTrainTracks
  , awakening
  , beginnersLuck
  , breakingAndEntering
  , campusSafety
  , cavernOfTheForgottenAge
  , closeTheRift
  , crossingTheThreshold
  , curtainCall
  , descentIntoDark
  , discoveringTheTruth
  , disruptingTheRitual
  , escapeTheRuins
  , exploringPnakotus
  , exploringTheRainforest
  , findTheRelic
  , findingANewWay
  , findingAWayInside
  , findingLadyEsprit
  , fold
  , friendsInHighPlacesHenryDeveau
  , friendsInHighPlacesHenrysInformation
  , getTheEngineRunning
  , getToTheBoats
  , harlanIsInDanger
  , harlansCurseHarlanEarnstone
  , harlansCurseSafekeeping
  , huntingTheRougarou
  , huntressOfTheEztli
  , intoTheBeyond
  , intoTheDarkness
  , intoTheRuins
  , intoTheRuinsOnceAgain
  , inLostCarcosa
  , investigatingTheTrail
  , journeyToTheNexus
  , leadingTheWay
  , magicAndScience
  , mendTheShatter
  , missingPersons
  , mistakesOfThePast
  , momentOfDoom
  , mysteriousGateway
  , nightAtTheMuseum
  , noAsylum
  , openingTheMaw
  , openThePathAbove
  , openThePathBelow
  , outOfThisWorld
  , paradiseLost
  , pastAndPresent
  , planningTheEscape
  , pursuingShadows
  , raceForAnswers
  , recoverTheRelic
  , repossession
  , restrictedAccess
  , ricesWhereabouts
  , row
  , run
  , saracenicScript
  , searchForAlejandro
  , searchForTheBrotherhood
  , searchForThePattern
  , searchForTheRuins
  , searchForTheStrangerV1
  , searchForTheStrangerV2
  , searchForTheStrangerV3
  , searchingForAnswers
  , searchingForTheTome
  , skinGame
  , stalkedByShadows
  , strangeOccurences
  , strangeRelicsMariaDeSilva
  , strangeRelicsMariasInformation
  , theBarrier
  , theBrotherhoodIsRevealed
  , theCarnevaleConspiracy
  , theCaveOfDarknessEmbroiledInBattle
  , theCaveOfDarknessTunnelsInTheDark
  , theChamberOfStillRemains
  , theChamberOfTheBeast
  , theDisappearance
  , theGateOpens
  , theGuardedRuins
  , theGuardiansInquiry
  , theKingInTatters
  , theOath
  , theParisianConspiracyV1
  , theParisianConspiracyV2
  , thePathIsBarred
  , thePathToTheHill
  , theReallyBadOnesV1
  , theReallyBadOnesV2
  , theRelicIsMissing
  , theReturnTrip
  , theStrangerACityAflame
  , theStrangerThePathIsMine
  , theStrangerTheShoresOfHali
  , theWayOut
  , theYithianRelic
  , theyMustBeDestroyed
  , throughTheCatacombs
  , timelock
  , trapped
  , trialOfTheHuntress
  , uncoveringTheConspiracy
  , whatHaveYouDone
  , worldsBeyond
  ]

trapped :: CardDef 'ActType
trapped = act "01108" "Trapped" 1 TheGathering

theBarrier :: CardDef 'ActType
theBarrier = act "01109" "The Barrier" 2 TheGathering

whatHaveYouDone :: CardDef 'ActType
whatHaveYouDone = act "01110" "What Have You Done?" 3 TheGathering

uncoveringTheConspiracy :: CardDef 'ActType
uncoveringTheConspiracy =
  act "01123" "Uncovering the Conspiracy" 1 TheMidnightMasks

investigatingTheTrail :: CardDef 'ActType
investigatingTheTrail =
  act "01146" "Investigating the Trail" 1 TheDevourerBelow

intoTheDarkness :: CardDef 'ActType
intoTheDarkness = act "01147" "Into the Darkness" 2 TheDevourerBelow

disruptingTheRitual :: CardDef 'ActType
disruptingTheRitual = act "01148" "Disrupting the Ritual" 3 TheDevourerBelow

afterHours :: CardDef 'ActType
afterHours = act "02045" "After Hours" 1 ExtracurricularActivity

ricesWhereabouts :: CardDef 'ActType
ricesWhereabouts = act "02046" "Rice's Whereabouts" 2 ExtracurricularActivity

campusSafety :: CardDef 'ActType
campusSafety = act "02047" "Campus Safety" 3 ExtracurricularActivity

beginnersLuck :: CardDef 'ActType
beginnersLuck = act "02066" "Beginner's Luck" 1 TheHouseAlwaysWins

skinGame :: CardDef 'ActType
skinGame = act "02067" "Skin Game" 2 TheHouseAlwaysWins

allIn :: CardDef 'ActType
allIn = act "02068" "All In" 3 TheHouseAlwaysWins

fold :: CardDef 'ActType
fold = act "02069" "Fold" 3 TheHouseAlwaysWins

findingAWayInside :: CardDef 'ActType
findingAWayInside = act "02122" "Finding A Way Inside" 1 TheMiskatonicMuseum

nightAtTheMuseum :: CardDef 'ActType
nightAtTheMuseum = act "02123" "Night at the Museum" 2 TheMiskatonicMuseum

breakingAndEntering :: CardDef 'ActType
breakingAndEntering = act "02124" "Breaking and Entering" 2 TheMiskatonicMuseum

searchingForTheTome :: CardDef 'ActType
searchingForTheTome =
  act "02125" "Searching for the Tome" 3 TheMiskatonicMuseum

run :: CardDef 'ActType
run = act "02165" "Run!" 1 TheEssexCountyExpress

getTheEngineRunning :: CardDef 'ActType
getTheEngineRunning =
  act "02166" "Get the Engine Running!" 2 TheEssexCountyExpress

searchingForAnswers :: CardDef 'ActType
searchingForAnswers = act "02199" "Searching for Answers" 1 BloodOnTheAltar

theChamberOfTheBeast :: CardDef 'ActType
theChamberOfTheBeast = act "02200" "The Chamber of the Beast" 2 BloodOnTheAltar

saracenicScript :: CardDef 'ActType
saracenicScript = act "02240" "Saracenic Script" 1 UndimensionedAndUnseen

theyMustBeDestroyed :: CardDef 'ActType
theyMustBeDestroyed =
  act "02241" "They Must Be Destroyed!" 2 UndimensionedAndUnseen

thePathToTheHill :: CardDef 'ActType
thePathToTheHill = act "02277" "The Path to the Hill" 1 WhereDoomAwaits

ascendingTheHillV1 :: CardDef 'ActType
ascendingTheHillV1 = act "02278" "Ascending the Hill (v. I)" 2 WhereDoomAwaits

ascendingTheHillV2 :: CardDef 'ActType
ascendingTheHillV2 = act "02279" "Ascending the Hill (v. II)" 2 WhereDoomAwaits

ascendingTheHillV3 :: CardDef 'ActType
ascendingTheHillV3 =
  act "02280" "Ascending the Hill (v. III)" 2 WhereDoomAwaits

theGateOpens :: CardDef 'ActType
theGateOpens = act "02281" "The Gate Opens" 3 WhereDoomAwaits

outOfThisWorld :: CardDef 'ActType
outOfThisWorld = act "02316" "Out of this World" 1 LostInTimeAndSpace

intoTheBeyond :: CardDef 'ActType
intoTheBeyond = act "02317" "Into the Beyond" 2 LostInTimeAndSpace

closeTheRift :: CardDef 'ActType
closeTheRift = act "02318" "Close the Rift" 3 LostInTimeAndSpace

findingANewWay :: CardDef 'ActType
findingANewWay = act "02319" "Finding a New Way" 4 LostInTimeAndSpace

awakening :: CardDef 'ActType
awakening = act "03046" "Awakening" 1 CurtainCall

theStrangerACityAflame :: CardDef 'ActType
theStrangerACityAflame = act "03047a" "The Stranger" 2 CurtainCall

theStrangerThePathIsMine :: CardDef 'ActType
theStrangerThePathIsMine = act "03047b" "The Stranger" 2 CurtainCall

theStrangerTheShoresOfHali :: CardDef 'ActType
theStrangerTheShoresOfHali = act "03047c" "The Stranger" 2 CurtainCall

curtainCall :: CardDef 'ActType
curtainCall = act "03048" "Curtain Call" 3 CurtainCall

discoveringTheTruth :: CardDef 'ActType
discoveringTheTruth = act "03064" "Discovering the Truth" 1 TheLastKing

raceForAnswers :: CardDef 'ActType
raceForAnswers = act "03124" "Race for Answers" 1 EchoesOfThePast

mistakesOfThePast :: CardDef 'ActType
mistakesOfThePast = act "03125" "Mistakes of the Past" 2 EchoesOfThePast

theOath :: CardDef 'ActType
theOath = act "03126" "The Oath" 3 EchoesOfThePast

arkhamAsylum :: CardDef 'ActType
arkhamAsylum = act "03163" "Arkham Asylum" 1 TheUnspeakableOath

theReallyBadOnesV1 :: CardDef 'ActType
theReallyBadOnesV1 =
  act "03164" "\"The Really Bad Ones\" (v. I)" 2 TheUnspeakableOath

theReallyBadOnesV2 :: CardDef 'ActType
theReallyBadOnesV2 =
  act "03165" "\"The Really Bad Ones\" (v. II)" 2 TheUnspeakableOath

planningTheEscape :: CardDef 'ActType
planningTheEscape = act "03166" "Planning the Escape" 3 TheUnspeakableOath

noAsylum :: CardDef 'ActType
noAsylum = act "03167" "No Asylum" 4 TheUnspeakableOath

theParisianConspiracyV1 :: CardDef 'ActType
theParisianConspiracyV1 =
  act "03204" "The Parisian Conspiracy (v. I)" 1 APhantomOfTruth

theParisianConspiracyV2 :: CardDef 'ActType
theParisianConspiracyV2 =
  act "03205" "The Parisian Conspiracy (v. II)" 1 APhantomOfTruth

pursuingShadows :: CardDef 'ActType
pursuingShadows = act "03206" "Pursuing Shadows" 2 APhantomOfTruth

stalkedByShadows :: CardDef 'ActType
stalkedByShadows = act "03207" "Stalked by Shadows" 2 APhantomOfTruth

throughTheCatacombs :: CardDef 'ActType
throughTheCatacombs = act "03243" "Through the Catacombs" 1 ThePallidMask

thePathIsBarred :: CardDef 'ActType
thePathIsBarred = act "03244" "The Path is Barred" 2 ThePallidMask

theWayOut :: CardDef 'ActType
theWayOut = act "03245" "The Way Out" 3 ThePallidMask

leadingTheWay :: CardDef 'ActType
leadingTheWay = act "03246" "Leading the Way" 3 ThePallidMask

openThePathBelow :: CardDef 'ActType
openThePathBelow = act "03281" "Open The Path Below" 3 BlackStarsRise

openThePathAbove :: CardDef 'ActType
openThePathAbove = act "03282" "Open The Path Above" 3 BlackStarsRise

inLostCarcosa :: CardDef 'ActType
inLostCarcosa = act "03320" "In Lost Carcosa" 1 DimCarcosa

searchForTheStrangerV1 :: CardDef 'ActType
searchForTheStrangerV1 =
  act "03321" "Search For the Stranger (v.I)" 2 DimCarcosa

searchForTheStrangerV2 :: CardDef 'ActType
searchForTheStrangerV2 =
  act "03322" "Search For the Stranger (v.II)" 2 DimCarcosa

searchForTheStrangerV3 :: CardDef 'ActType
searchForTheStrangerV3 =
  act "03323" "Search For the Stranger (v.III)" 2 DimCarcosa

theKingInTatters :: CardDef 'ActType
theKingInTatters = act "03324" "The King in Tatters" 3 DimCarcosa

exploringTheRainforest :: CardDef 'ActType
exploringTheRainforest =
  act "04046" "Exploring the Rainforest" 1 TheUntamedWilds

huntressOfTheEztli :: CardDef 'ActType
huntressOfTheEztli = act "04047" "Huntress of the Eztli" 2 TheUntamedWilds

searchForTheRuins :: CardDef 'ActType
searchForTheRuins = act "04048" "Search for the Ruins" 3 TheUntamedWilds

theGuardedRuins :: CardDef 'ActType
theGuardedRuins = act "04049" "The Guarded Ruins" 3 TheUntamedWilds

-- vengeance does not exist unless in victory pile, but this simplifies the logic
intoTheRuins :: CardDef 'ActType
intoTheRuins =
  (act "04057" "Into the Ruins" 1 TheDoomOfEztli) { cdVengeancePoints = Just 1 }

magicAndScience :: CardDef 'ActType
magicAndScience = act "04058" "Magic and Science" 2 TheDoomOfEztli

escapeTheRuins :: CardDef 'ActType
escapeTheRuins = act "04059" "Escape the Ruins" 3 TheDoomOfEztli

theRelicIsMissing :: CardDef 'ActType
theRelicIsMissing = act "04117" "The Relic is Missing!" 1 ThreadsOfFate

harlanIsInDanger :: CardDef 'ActType
harlanIsInDanger = act "04118" "Harlan is in Danger!" 1 ThreadsOfFate

atTheExhibitTheRelicsLocation :: CardDef 'ActType
atTheExhibitTheRelicsLocation = act "04119" "At the Exhibit" 2 ThreadsOfFate

atTheExhibitTheBrotherhoodsPlot :: CardDef 'ActType
atTheExhibitTheBrotherhoodsPlot = act "04120" "At the Exhibit" 2 ThreadsOfFate

harlansCurseSafekeeping :: CardDef 'ActType
harlansCurseSafekeeping = act "04121" "Harlan's Curse" 2 ThreadsOfFate

harlansCurseHarlanEarnstone :: CardDef 'ActType
harlansCurseHarlanEarnstone = act "04122" "Harlan's Curse" 2 ThreadsOfFate

findTheRelic :: CardDef 'ActType
findTheRelic = act "04123" "Find the Relic" 3 ThreadsOfFate

recoverTheRelic :: CardDef 'ActType
recoverTheRelic = act "04124" "Recover the Relic" 3 ThreadsOfFate

searchForAlejandro :: CardDef 'ActType
searchForAlejandro = act "04125c" "Search for Alejandro" 1 ThreadsOfFate

missingPersons :: CardDef 'ActType
missingPersons = act "04126c" "Missing Persons" 1 ThreadsOfFate

atTheStationInShadowedTalons :: CardDef 'ActType
atTheStationInShadowedTalons = act "04127c" "At the Station" 2 ThreadsOfFate

atTheStationTrainTracks :: CardDef 'ActType
atTheStationTrainTracks = act "04128c" "At the Station" 2 ThreadsOfFate

friendsInHighPlacesHenrysInformation :: CardDef 'ActType
friendsInHighPlacesHenrysInformation = act "04129c" "Friends in High Places" 2 ThreadsOfFate

friendsInHighPlacesHenryDeveau :: CardDef 'ActType
friendsInHighPlacesHenryDeveau = act "04130c" "Friends in High Places" 2 ThreadsOfFate

alejandrosPrison :: CardDef 'ActType
alejandrosPrison = act "04131c" "Alejandro's Prison" 3 ThreadsOfFate

alejandrosPlight :: CardDef 'ActType
alejandrosPlight = act "04132c" "Alejandro's Plight" 3 ThreadsOfFate

trialOfTheHuntress :: CardDef 'ActType
trialOfTheHuntress = act "04133e" "Trial of the Huntress" 1 ThreadsOfFate

theGuardiansInquiry :: CardDef 'ActType
theGuardiansInquiry = act "04134e" "The Guardian's Inquiry" 1 ThreadsOfFate

theCaveOfDarknessEmbroiledInBattle :: CardDef 'ActType
theCaveOfDarknessEmbroiledInBattle = act "04135e" "The Cave of Darkness" 2 ThreadsOfFate

theCaveOfDarknessTunnelsInTheDark :: CardDef 'ActType
theCaveOfDarknessTunnelsInTheDark = act "04136e" "The Cave of Darkness" 2 ThreadsOfFate

strangeRelicsMariaDeSilva :: CardDef 'ActType
strangeRelicsMariaDeSilva = act "04137e" "Strange Relics" 2 ThreadsOfFate

strangeRelicsMariasInformation :: CardDef 'ActType
strangeRelicsMariasInformation = act "04138e" "Strange Relics" 2 ThreadsOfFate

strangeOccurences :: CardDef 'ActType
strangeOccurences = act "04139e" "Strange Occurrences" 3 ThreadsOfFate

theBrotherhoodIsRevealed :: CardDef 'ActType
theBrotherhoodIsRevealed = act "04140e" "The Brotherhood is Revealed" 3 ThreadsOfFate

crossingTheThreshold :: CardDef 'ActType
crossingTheThreshold = act "04165" "Crossing the Threshold" 1 TheBoundaryBeyond

pastAndPresent :: CardDef 'ActType
pastAndPresent = act "04166" "Past and Present" 2 TheBoundaryBeyond

theReturnTrip :: CardDef 'ActType
theReturnTrip = act "04167" "The Return Trip" 3 TheBoundaryBeyond

searchForThePattern :: CardDef 'ActType
searchForThePattern = act "04209" "Search for the Pattern" 1 PillarsOfJudgement

openingTheMaw :: CardDef 'ActType
openingTheMaw = act "04210" "Opening the Maw" 2 PillarsOfJudgement

cavernOfTheForgottenAge :: CardDef 'ActType
cavernOfTheForgottenAge = act "04213" "Cavern of the Forgotten Age" 1 KnYan

descentIntoDark :: CardDef 'ActType
descentIntoDark = act "04214" "Descent into Dark" 2 KnYan

exploringPnakotus :: CardDef 'ActType
exploringPnakotus = act "04241" "Exploring Pnakotus" 1 TheCityOfArchives

restrictedAccess :: CardDef 'ActType
restrictedAccess = act "04242" "Restricted Access" 2 TheCityOfArchives

repossession :: CardDef 'ActType
repossession = act "04243" "Repossession" 3 TheCityOfArchives

journeyToTheNexus :: CardDef 'ActType
journeyToTheNexus = act "04285" "Journey to the Nexus" 1 TheDepthsOfYoth

worldsBeyond :: CardDef 'ActType
worldsBeyond = act "04318" "Worlds Beyond" 1 ShatteredAeons

searchForTheBrotherhood :: CardDef 'ActType
searchForTheBrotherhood = act "04319" "Search for the Brotherhood" 2 ShatteredAeons

theYithianRelic :: CardDef 'ActType
theYithianRelic = act "04320" "The Yithian Relic" 3 ShatteredAeons

mendTheShatter :: CardDef 'ActType
mendTheShatter = act "04321" "Mend the Shatter" 4 ShatteredAeons

paradiseLost :: CardDef 'ActType
paradiseLost = act "04322" "Paradise Lost" 4 ShatteredAeons

timelock :: CardDef 'ActType
timelock = act "04323" "Timelock" 4 ShatteredAeons

intoTheRuinsOnceAgain :: CardDef 'ActType
intoTheRuinsOnceAgain = (act "04345" "Into the Ruins Once Again" 1 TurnBackTime) { cdVengeancePoints = Just 2 }

theChamberOfStillRemains :: CardDef 'ActType
theChamberOfStillRemains = (act "04346" "The Chamber of Still Remains" 2 TurnBackTime) { cdVengeancePoints = Just 2 }

momentOfDoom :: CardDef 'ActType
momentOfDoom = act "04347" "Moment of Doom" 3 TurnBackTime

theDisappearance :: CardDef 'ActType
theDisappearance = act "05045" "The Disappearance" 1 DisappearanceAtTheTwilightEstate

mysteriousGateway :: CardDef 'ActType
mysteriousGateway = act "50012" "Mysterious Gateway" 1 ReturnToTheGathering

findingLadyEsprit :: CardDef 'ActType
findingLadyEsprit = act "81005" "Finding Lady Esprit" 1 TheBayou

huntingTheRougarou :: CardDef 'ActType
huntingTheRougarou = act "81006" "Hunting the Rougarou" 2 TheBayou

theCarnevaleConspiracy :: CardDef 'ActType
theCarnevaleConspiracy =
  act "82005" "The Carnevale Conspiracy" 1 CarnevaleOfHorrors

getToTheBoats :: CardDef 'ActType
getToTheBoats = act "82006" "Get to the Boats!" 2 CarnevaleOfHorrors

row :: CardDef 'ActType
row = act "82007" "Row!" 3 CarnevaleOfHorrors
