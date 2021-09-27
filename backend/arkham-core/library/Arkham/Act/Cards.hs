module Arkham.Act.Cards where

import Arkham.Prelude hiding (fold)

import Arkham.Types.Asset.Uses
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.CardType
import Arkham.Types.EncounterSet
import Arkham.Types.Name

act :: CardCode -> Name -> EncounterSet -> CardDef
act cardCode name encounterSet = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = ActType
  , cdCardSubType = Nothing
  , cdClassSymbol = Nothing
  , cdSkills = mempty
  , cdCardTraits = mempty
  , cdRevealedCardTraits = mempty
  , cdKeywords = mempty
  , cdFastWindow = Nothing
  , cdAction = Nothing
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdCriteria = mempty
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
  }

allActCards :: HashMap CardCode CardDef
allActCards = mapFromList $ map
  (toCardCode &&& id)
  [ trapped
  , theBarrier
  , whatHaveYouDone
  , uncoveringTheConspiracy
  , investigatingTheTrail
  , intoTheDarkness
  , disruptingTheRitual
  , afterHours
  , ricesWhereabouts
  , campusSafety
  , beginnersLuck
  , skinGame
  , allIn
  , fold
  , findingAWayInside
  , nightAtTheMuseum
  , breakingAndEntering
  , searchingForTheTome
  , run
  , getTheEngineRunning
  , searchingForAnswers
  , theChamberOfTheBeast
  , saracenicScript
  , theyMustBeDestroyed
  , thePathToTheHill
  , ascendingTheHillV1
  , ascendingTheHillV2
  , ascendingTheHillV3
  , theGateOpens
  , outOfThisWorld
  , intoTheBeyond
  , closeTheRift
  , findingANewWay
  , awakening
  , theStrangerACityAflame
  , theStrangerThePathIsMine
  , theStrangerTheShoresOfHali
  , curtainCall
  , discoveringTheTruth
  , raceForAnswers
  , mistakesOfThePast
  , theOath
  , mysteriousGateway
  , findingLadyEsprit
  , huntingTheRougarou
  , theCarnevaleConspiracy
  , getToTheBoats
  , row
  ]

trapped :: CardDef
trapped = act "01108" "Trapped" TheGathering

theBarrier :: CardDef
theBarrier = act "01109" "The Barrier" TheGathering

whatHaveYouDone :: CardDef
whatHaveYouDone = act "01110" "What Have You Done?" TheGathering

uncoveringTheConspiracy :: CardDef
uncoveringTheConspiracy =
  act "01123" "Uncovering the Conspiracy" TheMidnightMasks

investigatingTheTrail :: CardDef
investigatingTheTrail = act "01146" "Investigating the Trail" TheDevourerBelow

intoTheDarkness :: CardDef
intoTheDarkness = act "01147" "Into the Darkness" TheDevourerBelow

disruptingTheRitual :: CardDef
disruptingTheRitual = act "01148" "Disrupting the Ritual" TheDevourerBelow

afterHours :: CardDef
afterHours = act "02045" "After Hours" ExtracurricularActivity

ricesWhereabouts :: CardDef
ricesWhereabouts = act "02046" "Rice's Whereabouts" ExtracurricularActivity

campusSafety :: CardDef
campusSafety = act "02047" "Campus Safety" ExtracurricularActivity

beginnersLuck :: CardDef
beginnersLuck = act "02066" "Beginner's Luck" TheHouseAlwaysWins

skinGame :: CardDef
skinGame = act "02067" "Skin Game" TheHouseAlwaysWins

allIn :: CardDef
allIn = act "02068" "All In" TheHouseAlwaysWins

fold :: CardDef
fold = act "02069" "Fold" TheHouseAlwaysWins

findingAWayInside :: CardDef
findingAWayInside = act "02122" "Finding A Way Inside" TheMiskatonicMuseum

nightAtTheMuseum :: CardDef
nightAtTheMuseum = act "02123" "Night at the Museum" TheMiskatonicMuseum

breakingAndEntering :: CardDef
breakingAndEntering = act "02124" "Breaking and Entering" TheMiskatonicMuseum

searchingForTheTome :: CardDef
searchingForTheTome = act "02125" "Searching for the Tome" TheMiskatonicMuseum

run :: CardDef
run = act "02165" "Run!" TheEssexCountyExpress

getTheEngineRunning :: CardDef
getTheEngineRunning =
  act "02166" "Get the Engine Running!" TheEssexCountyExpress

searchingForAnswers :: CardDef
searchingForAnswers = act "02199" "Searching for Answers" BloodOnTheAltar

theChamberOfTheBeast :: CardDef
theChamberOfTheBeast = act "02200" "The Chamber of the Beast" BloodOnTheAltar

saracenicScript :: CardDef
saracenicScript = act "02240" "Saracenic Script" UndimensionedAndUnseen

theyMustBeDestroyed :: CardDef
theyMustBeDestroyed =
  act "02241" "They Must Be Destroyed!" UndimensionedAndUnseen

thePathToTheHill :: CardDef
thePathToTheHill = act "02277" "The Path to the Hill" WhereDoomAwaits

ascendingTheHillV1 :: CardDef
ascendingTheHillV1 = act "02278" "Ascending the Hill (v. I)" WhereDoomAwaits

ascendingTheHillV2 :: CardDef
ascendingTheHillV2 = act "02279" "Ascending the Hill (v. II)" WhereDoomAwaits

ascendingTheHillV3 :: CardDef
ascendingTheHillV3 = act "02280" "Ascending the Hill (v. III)" WhereDoomAwaits

theGateOpens :: CardDef
theGateOpens = act "02281" "The Gate Opens" WhereDoomAwaits

outOfThisWorld :: CardDef
outOfThisWorld = act "02316" "Out of this World" LostInTimeAndSpace

intoTheBeyond :: CardDef
intoTheBeyond = act "02317" "Into the Beyond" LostInTimeAndSpace

closeTheRift :: CardDef
closeTheRift = act "02318" "Close the Rift" LostInTimeAndSpace

findingANewWay :: CardDef
findingANewWay = act "02319" "Finding a New Way" LostInTimeAndSpace

awakening :: CardDef
awakening = act "03046" "Awakening" CurtainCall

theStrangerACityAflame :: CardDef
theStrangerACityAflame = act "03047a" "The Stranger" CurtainCall

theStrangerThePathIsMine :: CardDef
theStrangerThePathIsMine = act "03047b" "The Stranger" CurtainCall

theStrangerTheShoresOfHali :: CardDef
theStrangerTheShoresOfHali = act "03047c" "The Stranger" CurtainCall

curtainCall :: CardDef
curtainCall = act "03048" "Curtain Call" CurtainCall

discoveringTheTruth :: CardDef
discoveringTheTruth = act "03064" "Discovering the Truth" TheLastKing

raceForAnswers :: CardDef
raceForAnswers = act "03124" "Race for Answers" EchoesOfThePast

mistakesOfThePast :: CardDef
mistakesOfThePast = act "03125" "Mistakes of the Past" EchoesOfThePast

theOath :: CardDef
theOath = act "03126" "The Oath" EchoesOfThePast

arkhamAsylum :: CardDef
arkhamAsylum = act "03163" "Arkham Asylum" TheUnspeakableOath

theReallyBadOnesV1 :: CardDef
theReallyBadOnesV1 =
  act "03164" "\"The Really Bad Ones\" (v. I)" TheUnspeakableOath

theReallyBadOnesV2 :: CardDef
theReallyBadOnesV2 =
  act "03165" "\"The Really Bad Ones\" (v. II)" TheUnspeakableOath

planningTheEscape :: CardDef
planningTheEscape = act "03166" "Planning the Escape" TheUnspeakableOath

noAsylum :: CardDef
noAsylum = act "03167" "No Asylum" TheUnspeakableOath

mysteriousGateway :: CardDef
mysteriousGateway = act "50012" "Mysterious Gateway" ReturnToTheGathering

findingLadyEsprit :: CardDef
findingLadyEsprit = act "81005" "Finding Lady Esprit" TheBayou

huntingTheRougarou :: CardDef
huntingTheRougarou = act "81006" "Hunting the Rougarou" TheBayou

theCarnevaleConspiracy :: CardDef
theCarnevaleConspiracy =
  act "82005" "The Carnevale Conspiracy" CarnevaleOfHorrors

getToTheBoats :: CardDef
getToTheBoats = act "82006" "Get to the Boats!" CarnevaleOfHorrors

row :: CardDef
row = act "82007" "Row!" CarnevaleOfHorrors
