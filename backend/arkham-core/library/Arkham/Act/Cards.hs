module Arkham.Act.Cards where

import Arkham.Prelude hiding ( fold )

import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name

act :: CardCode -> Name -> Int -> EncounterSet -> CardDef
act cardCode name stage encounterSet = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = ActType
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
  }

allActCards :: HashMap CardCode CardDef
allActCards = mapFromList $ map
  (toCardCode &&& id)
  [ afterHours
  , allIn
  , arkhamAsylum
  , ascendingTheHillV1
  , ascendingTheHillV2
  , ascendingTheHillV3
  , awakening
  , beginnersLuck
  , breakingAndEntering
  , campusSafety
  , closeTheRift
  , curtainCall
  , discoveringTheTruth
  , disruptingTheRitual
  , escapeTheRuins
  , exploringTheRainforest
  , findingANewWay
  , findingAWayInside
  , findingLadyEsprit
  , fold
  , getTheEngineRunning
  , getToTheBoats
  , huntingTheRougarou
  , huntressOfTheEztli
  , intoTheBeyond
  , intoTheDarkness
  , intoTheRuins
  , inLostCarcosa
  , investigatingTheTrail
  , leadingTheWay
  , magicAndScience
  , mistakesOfThePast
  , mysteriousGateway
  , nightAtTheMuseum
  , noAsylum
  , openThePathAbove
  , openThePathBelow
  , outOfThisWorld
  , planningTheEscape
  , pursuingShadows
  , raceForAnswers
  , ricesWhereabouts
  , row
  , run
  , saracenicScript
  , searchForTheRuins
  , searchForTheStrangerV1
  , searchForTheStrangerV2
  , searchForTheStrangerV3
  , searchingForAnswers
  , searchingForTheTome
  , skinGame
  , stalkedByShadows
  , theBarrier
  , theCarnevaleConspiracy
  , theChamberOfTheBeast
  , theGateOpens
  , theGuardedRuins
  , theKingInTatters
  , theOath
  , theParisianConspiracyV1
  , theParisianConspiracyV2
  , thePathIsBarred
  , thePathToTheHill
  , theReallyBadOnesV1
  , theReallyBadOnesV2
  , theStrangerACityAflame
  , theStrangerThePathIsMine
  , theStrangerTheShoresOfHali
  , theWayOut
  , theyMustBeDestroyed
  , throughTheCatacombs
  , trapped
  , uncoveringTheConspiracy
  , whatHaveYouDone
  ]

trapped :: CardDef
trapped = act "01108" "Trapped" 1 TheGathering

theBarrier :: CardDef
theBarrier = act "01109" "The Barrier" 2 TheGathering

whatHaveYouDone :: CardDef
whatHaveYouDone = act "01110" "What Have You Done?" 3 TheGathering

uncoveringTheConspiracy :: CardDef
uncoveringTheConspiracy =
  act "01123" "Uncovering the Conspiracy" 1 TheMidnightMasks

investigatingTheTrail :: CardDef
investigatingTheTrail =
  act "01146" "Investigating the Trail" 1 TheDevourerBelow

intoTheDarkness :: CardDef
intoTheDarkness = act "01147" "Into the Darkness" 2 TheDevourerBelow

disruptingTheRitual :: CardDef
disruptingTheRitual = act "01148" "Disrupting the Ritual" 3 TheDevourerBelow

afterHours :: CardDef
afterHours = act "02045" "After Hours" 1 ExtracurricularActivity

ricesWhereabouts :: CardDef
ricesWhereabouts = act "02046" "Rice's Whereabouts" 2 ExtracurricularActivity

campusSafety :: CardDef
campusSafety = act "02047" "Campus Safety" 3 ExtracurricularActivity

beginnersLuck :: CardDef
beginnersLuck = act "02066" "Beginner's Luck" 1 TheHouseAlwaysWins

skinGame :: CardDef
skinGame = act "02067" "Skin Game" 2 TheHouseAlwaysWins

allIn :: CardDef
allIn = act "02068" "All In" 3 TheHouseAlwaysWins

fold :: CardDef
fold = act "02069" "Fold" 3 TheHouseAlwaysWins

findingAWayInside :: CardDef
findingAWayInside = act "02122" "Finding A Way Inside" 1 TheMiskatonicMuseum

nightAtTheMuseum :: CardDef
nightAtTheMuseum = act "02123" "Night at the Museum" 2 TheMiskatonicMuseum

breakingAndEntering :: CardDef
breakingAndEntering = act "02124" "Breaking and Entering" 2 TheMiskatonicMuseum

searchingForTheTome :: CardDef
searchingForTheTome =
  act "02125" "Searching for the Tome" 3 TheMiskatonicMuseum

run :: CardDef
run = act "02165" "Run!" 1 TheEssexCountyExpress

getTheEngineRunning :: CardDef
getTheEngineRunning =
  act "02166" "Get the Engine Running!" 2 TheEssexCountyExpress

searchingForAnswers :: CardDef
searchingForAnswers = act "02199" "Searching for Answers" 1 BloodOnTheAltar

theChamberOfTheBeast :: CardDef
theChamberOfTheBeast = act "02200" "The Chamber of the Beast" 2 BloodOnTheAltar

saracenicScript :: CardDef
saracenicScript = act "02240" "Saracenic Script" 1 UndimensionedAndUnseen

theyMustBeDestroyed :: CardDef
theyMustBeDestroyed =
  act "02241" "They Must Be Destroyed!" 2 UndimensionedAndUnseen

thePathToTheHill :: CardDef
thePathToTheHill = act "02277" "The Path to the Hill" 1 WhereDoomAwaits

ascendingTheHillV1 :: CardDef
ascendingTheHillV1 = act "02278" "Ascending the Hill (v. I)" 2 WhereDoomAwaits

ascendingTheHillV2 :: CardDef
ascendingTheHillV2 = act "02279" "Ascending the Hill (v. II)" 2 WhereDoomAwaits

ascendingTheHillV3 :: CardDef
ascendingTheHillV3 =
  act "02280" "Ascending the Hill (v. III)" 2 WhereDoomAwaits

theGateOpens :: CardDef
theGateOpens = act "02281" "The Gate Opens" 3 WhereDoomAwaits

outOfThisWorld :: CardDef
outOfThisWorld = act "02316" "Out of this World" 1 LostInTimeAndSpace

intoTheBeyond :: CardDef
intoTheBeyond = act "02317" "Into the Beyond" 2 LostInTimeAndSpace

closeTheRift :: CardDef
closeTheRift = act "02318" "Close the Rift" 3 LostInTimeAndSpace

findingANewWay :: CardDef
findingANewWay = act "02319" "Finding a New Way" 4 LostInTimeAndSpace

awakening :: CardDef
awakening = act "03046" "Awakening" 1 CurtainCall

theStrangerACityAflame :: CardDef
theStrangerACityAflame = act "03047a" "The Stranger" 2 CurtainCall

theStrangerThePathIsMine :: CardDef
theStrangerThePathIsMine = act "03047b" "The Stranger" 2 CurtainCall

theStrangerTheShoresOfHali :: CardDef
theStrangerTheShoresOfHali = act "03047c" "The Stranger" 2 CurtainCall

curtainCall :: CardDef
curtainCall = act "03048" "Curtain Call" 3 CurtainCall

discoveringTheTruth :: CardDef
discoveringTheTruth = act "03064" "Discovering the Truth" 1 TheLastKing

raceForAnswers :: CardDef
raceForAnswers = act "03124" "Race for Answers" 1 EchoesOfThePast

mistakesOfThePast :: CardDef
mistakesOfThePast = act "03125" "Mistakes of the Past" 2 EchoesOfThePast

theOath :: CardDef
theOath = act "03126" "The Oath" 3 EchoesOfThePast

arkhamAsylum :: CardDef
arkhamAsylum = act "03163" "Arkham Asylum" 1 TheUnspeakableOath

theReallyBadOnesV1 :: CardDef
theReallyBadOnesV1 =
  act "03164" "\"The Really Bad Ones\" (v. I)" 2 TheUnspeakableOath

theReallyBadOnesV2 :: CardDef
theReallyBadOnesV2 =
  act "03165" "\"The Really Bad Ones\" (v. II)" 2 TheUnspeakableOath

planningTheEscape :: CardDef
planningTheEscape = act "03166" "Planning the Escape" 3 TheUnspeakableOath

noAsylum :: CardDef
noAsylum = act "03167" "No Asylum" 4 TheUnspeakableOath

theParisianConspiracyV1 :: CardDef
theParisianConspiracyV1 =
  act "03204" "The Parisian Conspiracy (v. I)" 1 APhantomOfTruth

theParisianConspiracyV2 :: CardDef
theParisianConspiracyV2 =
  act "03205" "The Parisian Conspiracy (v. II)" 1 APhantomOfTruth

pursuingShadows :: CardDef
pursuingShadows = act "03206" "Pursuing Shadows" 2 APhantomOfTruth

stalkedByShadows :: CardDef
stalkedByShadows = act "03207" "Stalked by Shadows" 2 APhantomOfTruth

throughTheCatacombs :: CardDef
throughTheCatacombs = act "03243" "Through the Catacombs" 1 ThePallidMask

thePathIsBarred :: CardDef
thePathIsBarred = act "03244" "The Path is Barred" 2 ThePallidMask

theWayOut :: CardDef
theWayOut = act "03245" "The Way Out" 3 ThePallidMask

leadingTheWay :: CardDef
leadingTheWay = act "03246" "Leading the Way" 3 ThePallidMask

openThePathBelow :: CardDef
openThePathBelow = act "03281" "Open The Path Below" 3 BlackStarsRise

openThePathAbove :: CardDef
openThePathAbove = act "03282" "Open The Path Above" 3 BlackStarsRise

inLostCarcosa :: CardDef
inLostCarcosa = act "03320" "In Lost Carcosa" 1 DimCarcosa

searchForTheStrangerV1 :: CardDef
searchForTheStrangerV1 =
  act "03321" "Search For The Stanger (v. I)" 2 DimCarcosa

searchForTheStrangerV2 :: CardDef
searchForTheStrangerV2 =
  act "03322" "Search For The Stanger (v. II)" 2 DimCarcosa

searchForTheStrangerV3 :: CardDef
searchForTheStrangerV3 =
  act "03323" "Search For The Stanger (v. III)" 2 DimCarcosa

theKingInTatters :: CardDef
theKingInTatters = act "03324" "The King in Tatters" 3 DimCarcosa

exploringTheRainforest :: CardDef
exploringTheRainforest =
  act "04046" "Exploring the Rainforest" 1 TheUntamedWilds

huntressOfTheEztli :: CardDef
huntressOfTheEztli = act "04047" "Huntress of the Eztli" 2 TheUntamedWilds

searchForTheRuins :: CardDef
searchForTheRuins = act "04048" "Search for the Ruins" 3 TheUntamedWilds

theGuardedRuins :: CardDef
theGuardedRuins = act "04049" "The Guarded Ruins" 3 TheUntamedWilds

-- vengeance does not exist unless in victory pile, but this simplifies the logic
intoTheRuins :: CardDef
intoTheRuins =
  (act "04057" "Into the Ruins" 1 TheDoomOfEztli) { cdVengeancePoints = Just 1 }

magicAndScience :: CardDef
magicAndScience = act "04058" "Magic and Science" 2 TheDoomOfEztli

escapeTheRuins :: CardDef
escapeTheRuins = act "04059" "Escape the Ruins" 3 TheDoomOfEztli

mysteriousGateway :: CardDef
mysteriousGateway = act "50012" "Mysterious Gateway" 1 ReturnToTheGathering

findingLadyEsprit :: CardDef
findingLadyEsprit = act "81005" "Finding Lady Esprit" 1 TheBayou

huntingTheRougarou :: CardDef
huntingTheRougarou = act "81006" "Hunting the Rougarou" 2 TheBayou

theCarnevaleConspiracy :: CardDef
theCarnevaleConspiracy =
  act "82005" "The Carnevale Conspiracy" 1 CarnevaleOfHorrors

getToTheBoats :: CardDef
getToTheBoats = act "82006" "Get to the Boats!" 2 CarnevaleOfHorrors

row :: CardDef
row = act "82007" "Row!" 3 CarnevaleOfHorrors
