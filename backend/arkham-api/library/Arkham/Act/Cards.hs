module Arkham.Act.Cards where

import Arkham.Prelude hiding (fold)

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name

act :: CardCode -> Name -> Int -> EncounterSet -> CardDef
act cardCode name stage encounterSet =
  (emptyCardDef cardCode name ActType)
    { cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Nothing
    , cdDoubleSided = True
    , cdStage = Just stage
    , cdLevel = Nothing
    }

allActCards :: Map CardCode CardDef
allActCards =
  mapFromList
    $ map
      (toCardCode &&& id)
      [ aCircleUnbroken
      , aFamiliarPattern
      , afterHours
      , alejandrosPlight
      , alejandrosPrison
      , allIn
      , arkhamAsylum
      , ascendTheMountain
      , ascendingTheHillV1
      , ascendingTheHillV2
      , ascendingTheHillV3
      , atTheExhibitTheBrotherhoodsPlot
      , atTheExhibitTheRelicsLocation
      , atTheStationInShadowedTalons
      , atTheStationTrainTracks
      , awakening
      , backIntoTheDepths
      , beginnersLuck
      , beyondDreams
      , beyondTheGrave
      , beyondTheMistV1
      , beyondTheMistV2
      , beyondTheMistV3
      , beyondTheMistV4
      , beyondTheWitchHouse
      , breakingAndEntering
      , campusSafety
      , cavernOfTheForgottenAge
      , cityOfTheDeepV1
      , cityOfTheDeepV2
      , cityOfTheDeepV3
      , closeTheRift
      , collapseThePylons
      , containingTheOutbreak
      , crossingTheThreshold
      , curtainCall
      , darkKnowledgeV1
      , darkKnowledgeV2
      , descentIntoDark
      , discoverTheTruth
      , discoveringTheTruth
      , disruptingTheRitual
      , dormancy
      , enteringTheDreamlands
      , enteringTheUnderworldV1
      , enteringTheUnderworldV2
      , escapeTheCage
      , escapeTheRuins
      , exploringPnakotus
      , exploringTheMoon
      , exploringTheRainforest
      , fatedSouls
      , findTheRelic
      , findingANewWay
      , findingAWayInside
      , findingLadyEsprit
      , findingThePath
      , findingTheJewel
      , fold
      , followingLeads
      , friendsInHighPlacesHenryDeveau
      , friendsInHighPlacesHenrysInformation
      , getTheEngineRunning
      , getToTheBoats
      , harlanIsInDanger
      , harlansCurseHarlanEarnstone
      , harlansCurseSafekeeping
      , hiddenAgendas
      , huntingTheRougarou
      , huntressOfTheEztli
      , impossiblePursuit
      , inAzathothsDomain
      , infiltratingTheLodge
      , intoTheBeyond
      , intoTheBeyondV2
      , intoTheDarkness
      , intoTheRuins
      , intoTheRuinsOnceAgain
      , investigatingTheWitchHouse
      , inLostCarcosa
      , inPursuitOfTheDead
      , inPursuitOfTheLiving
      , inTheBellyOfTheMoonBeast
      , investigatingTheTrail
      , journeyAcrossTheBridge
      , journeyThroughTheColdWastes
      , journeyToTheNexus
      , kingdomOfTheSkai
      , leadingTheWay
      , lookingForAnswers
      , lostInTheWoods
      , magicAndScience
      , meetAndGreet
      , mendTheShatter
      , missingPersons
      , mistakesOfThePast
      , momentOfDoom
      , mysteriousGateway
      , newWorldOrder
      , nightAtTheMuseum
      , noAsylum
      , obtainingTheDevice
      , openingTheMaw
      , openThePathAbove
      , openThePathBelow
      , outOfThisWorld
      , paradiseLost
      , pastAndPresent
      , pathsIntoTwilight
      , pedalToTheMetal
      , planningTheEscape
      , pursuingShadows
      , pursuitOfTheUnknownV1
      , pursuitOfTheUnknownV2
      , pursuitOfTheUnknownV3
      , raceForAnswers
      , recoverTheRelic
      , reefOfMysteries
      , repossession
      , restrictedAccess
      , ricesWhereabouts
      , row
      , run
      , saracenicScript
      , searchForACampSite
      , searchForAlejandro
      , searchForTheBrotherhood
      , searchForTheMeaning
      , searchForThePatient
      , searchForThePattern
      , searchForTheRuins
      , searchForTheSource
      , searchForTheStrangerV1
      , searchForTheStrangerV2
      , searchForTheStrangerV3
      , searchingForAnswers
      , searchingForTheTome
      , searchingTheUnnamable
      , seekOutTheNight
      , seekingTroubleLoadingDocks
      , seekingTroubleSentFromAnotherTime
      , shadowOfThePastV1
      , shadowOfThePastV2
      , shadowOfThePastV3
      , skinGame
      , sprawlingCityV1
      , sprawlingCityV2
      , sprawlingCityV3
      , stalkedByShadows
      , stoppingTheRitual
      , strangeOccurences
      , strangeRelicsMariaDeSilva
      , strangeRelicsMariasInformation
      , theBarrier
      , theBindingRite
      , theBlackExpanse
      , theBrokenRite
      , theBrotherhoodIsRevealed
      , theCarnevaleConspiracy
      , theCaveOfDarknessEmbroiledInBattle
      , theCaveOfDarknessTunnelsInTheDark
      , theChamberOfStillRemains
      , theChamberOfTheBeast
      , theCosmosBeckons
      , theDescent
      , theDisappearance
      , theDoomThatCameBefore
      , theDreamEaters
      , theEndlessStairs
      , theEscape
      , theFinalDescent
      , theFinalMirage
      , theFirstOath
      , theFourKeys
      , theGateOpens
      , theGreatSeal
      , theGuardedRuins
      , theGuardiansInquiry
      , theIsleOfOriab
      , theLighthouse
      , theLostExpedition
      , theKingInTatters
      , theKingsDecree
      , theMoonsCore
      , theOath
      , theOvergrownEstateClintonFreeman
      , theOvergrownEstateSentFromAnotherTime
      , theParisianConspiracyV1
      , theParisianConspiracyV2
      , thePath
      , thePathIsBarred
      , thePathToTheHill
      , thePit
      , theReallyBadOnesV1
      , theReallyBadOnesV2
      , theReallyBadOnesV3
      , theReallyBadOnesV4
      , theRelicIsMissing
      , theRescue
      , theReturnTrip
      , theSchemesDemise
      , theSearchForAgentHarper
      , theSecondOath
      , theSpectralRealm
      , theStrangerACityAflame
      , theStrangerAlaranMists
      , theStrangerHereIsMyReply
      , theStrangerThePathIsMine
      , theStrangerTheShoresOfHali
      , theStrangerUnderTheCity
      , theThingInTheRobes
      , theThirdOath
      , theTrialOfNashtAndKamanThah
      , theUnvisitedIsle
      , theWayOut
      , theWeaverOfTheCosmos
      , theYithianRelic
      , theyMustBeDestroyed
      , throughTheCatacombs
      , throughTheLabyrinth
      , timelock
      , trapped
      , trialOfTheHuntress
      , truthAndLies
      , uncoveringTheConspiracy
      , underAttack
      , unexpectedRescue
      , warmWelcome
      , whatHappened
      , whatHaveYouDone
      , whatMustBeDone
      , witchHauntings
      , worldsBeyond
      , worshippersOfTheDeep
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
  act "03321a" "Search For the Stranger (v.I)" 2 DimCarcosa

searchForTheStrangerV2 :: CardDef
searchForTheStrangerV2 =
  act "03322a" "Search For the Stranger (v.II)" 2 DimCarcosa

searchForTheStrangerV3 :: CardDef
searchForTheStrangerV3 =
  act "03323a" "Search For the Stranger (v.III)" 2 DimCarcosa

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
  (act "04057" "Into the Ruins" 1 TheDoomOfEztli) {cdVengeancePoints = Just 1}

magicAndScience :: CardDef
magicAndScience = act "04058" "Magic and Science" 2 TheDoomOfEztli

escapeTheRuins :: CardDef
escapeTheRuins = act "04059" "Escape the Ruins" 3 TheDoomOfEztli

theRelicIsMissing :: CardDef
theRelicIsMissing = act "04117" "The Relic Is Missing!" 1 ThreadsOfFate

harlanIsInDanger :: CardDef
harlanIsInDanger = act "04118" "Harlan Is in Danger!" 1 ThreadsOfFate

atTheExhibitTheRelicsLocation :: CardDef
atTheExhibitTheRelicsLocation = act "04119" "At the Exhibit" 2 ThreadsOfFate

atTheExhibitTheBrotherhoodsPlot :: CardDef
atTheExhibitTheBrotherhoodsPlot = act "04120" "At the Exhibit" 2 ThreadsOfFate

harlansCurseSafekeeping :: CardDef
harlansCurseSafekeeping = act "04121" "Harlan's Curse" 2 ThreadsOfFate

harlansCurseHarlanEarnstone :: CardDef
harlansCurseHarlanEarnstone = act "04122" "Harlan's Curse" 2 ThreadsOfFate

findTheRelic :: CardDef
findTheRelic = act "04123" "Find the Relic" 3 ThreadsOfFate

recoverTheRelic :: CardDef
recoverTheRelic = act "04124" "Recover the Relic" 3 ThreadsOfFate

searchForAlejandro :: CardDef
searchForAlejandro = act "04125" "Search for Alejandro" 1 ThreadsOfFate

missingPersons :: CardDef
missingPersons = act "04126" "Missing Persons" 1 ThreadsOfFate

atTheStationInShadowedTalons :: CardDef
atTheStationInShadowedTalons = act "04127" "At the Station" 2 ThreadsOfFate

atTheStationTrainTracks :: CardDef
atTheStationTrainTracks = act "04128" "At the Station" 2 ThreadsOfFate

friendsInHighPlacesHenrysInformation :: CardDef
friendsInHighPlacesHenrysInformation = act "04129" "Friends in High Places" 2 ThreadsOfFate

friendsInHighPlacesHenryDeveau :: CardDef
friendsInHighPlacesHenryDeveau = act "04130" "Friends in High Places" 2 ThreadsOfFate

alejandrosPrison :: CardDef
alejandrosPrison = act "04131" "Alejandro's Prison" 3 ThreadsOfFate

alejandrosPlight :: CardDef
alejandrosPlight = act "04132" "Alejandro's Plight" 3 ThreadsOfFate

trialOfTheHuntress :: CardDef
trialOfTheHuntress = act "04133" "Trial of the Huntress" 1 ThreadsOfFate

theGuardiansInquiry :: CardDef
theGuardiansInquiry = act "04134" "The Guardian's Inquiry" 1 ThreadsOfFate

theCaveOfDarknessEmbroiledInBattle :: CardDef
theCaveOfDarknessEmbroiledInBattle = act "04135" "The Cave of Darkness" 2 ThreadsOfFate

theCaveOfDarknessTunnelsInTheDark :: CardDef
theCaveOfDarknessTunnelsInTheDark = act "04136" "The Cave of Darkness" 2 ThreadsOfFate

strangeRelicsMariaDeSilva :: CardDef
strangeRelicsMariaDeSilva = act "04137" "Strange Relics" 2 ThreadsOfFate

strangeRelicsMariasInformation :: CardDef
strangeRelicsMariasInformation = act "04138" "Strange Relics" 2 ThreadsOfFate

strangeOccurences :: CardDef
strangeOccurences = act "04139" "Strange Occurrences" 3 ThreadsOfFate

theBrotherhoodIsRevealed :: CardDef
theBrotherhoodIsRevealed = act "04140" "The Brotherhood Is Revealed" 3 ThreadsOfFate

crossingTheThreshold :: CardDef
crossingTheThreshold = act "04165" "Crossing the Threshold" 1 TheBoundaryBeyond

pastAndPresent :: CardDef
pastAndPresent = act "04166" "Past and Present" 2 TheBoundaryBeyond

theReturnTrip :: CardDef
theReturnTrip = act "04167" "The Return Trip" 3 TheBoundaryBeyond

searchForThePattern :: CardDef
searchForThePattern = act "04209" "Search for the Pattern" 1 PillarsOfJudgement

openingTheMaw :: CardDef
openingTheMaw = act "04210" "Opening the Maw" 2 PillarsOfJudgement

cavernOfTheForgottenAge :: CardDef
cavernOfTheForgottenAge = act "04213" "Cavern of the Forgotten Age" 1 KnYan

descentIntoDark :: CardDef
descentIntoDark = act "04214" "Descent into Dark" 2 KnYan

exploringPnakotus :: CardDef
exploringPnakotus = act "04241" "Exploring Pnakotus" 1 TheCityOfArchives

restrictedAccess :: CardDef
restrictedAccess = act "04242" "Restricted Access" 2 TheCityOfArchives

repossession :: CardDef
repossession = act "04243" "Repossession" 3 TheCityOfArchives

journeyToTheNexus :: CardDef
journeyToTheNexus = act "04285" "Journey to the Nexus" 1 TheDepthsOfYoth

worldsBeyond :: CardDef
worldsBeyond = act "04318" "Worlds Beyond" 1 ShatteredAeons

searchForTheBrotherhood :: CardDef
searchForTheBrotherhood = act "04319" "Search for the Brotherhood" 2 ShatteredAeons

theYithianRelic :: CardDef
theYithianRelic = act "04320" "The Yithian Relic" 3 ShatteredAeons

mendTheShatter :: CardDef
mendTheShatter = act "04321" "Mend the Shatter" 4 ShatteredAeons

paradiseLost :: CardDef
paradiseLost = act "04322" "Paradise Lost" 4 ShatteredAeons

timelock :: CardDef
timelock = act "04323" "Timelock" 4 ShatteredAeons

intoTheRuinsOnceAgain :: CardDef
intoTheRuinsOnceAgain = (act "04345" "Into the Ruins Once Again" 1 TurnBackTime) {cdVengeancePoints = Just 2}

theChamberOfStillRemains :: CardDef
theChamberOfStillRemains = (act "04346" "The Chamber of Still Remains" 2 TurnBackTime) {cdVengeancePoints = Just 2}

momentOfDoom :: CardDef
momentOfDoom = act "04347" "Moment of Doom" 3 TurnBackTime

theDisappearance :: CardDef
theDisappearance = act "05045" "The Disappearance" 1 DisappearanceAtTheTwilightEstate

lostInTheWoods :: CardDef
lostInTheWoods = act "05053" "Lost in the Woods" 1 TheWitchingHour

witchHauntings :: CardDef
witchHauntings = act "05054" "Witch Hauntings" 2 TheWitchingHour

pathsIntoTwilight :: CardDef
pathsIntoTwilight = act "05055" "Paths into Twilight" 3 TheWitchingHour

aCircleUnbroken :: CardDef
aCircleUnbroken = act "05056" "A Circle Unbroken" 4 TheWitchingHour

hiddenAgendas :: CardDef
hiddenAgendas = act "05068" "Hidden Agendas" 1 AtDeathsDoorstep

theSpectralRealm :: CardDef
theSpectralRealm = act "05069" "The Spectral Realm" 2 AtDeathsDoorstep

escapeTheCage :: CardDef
escapeTheCage = act "05070" "Escape the Cage" 3 AtDeathsDoorstep

investigatingTheWitchHouse :: CardDef
investigatingTheWitchHouse = act "05125" "Investigating the Witch House" 1 TheSecretName

beyondTheWitchHouse :: CardDef
beyondTheWitchHouse = act "05126" "Beyond the Witch House" 2 TheSecretName

stoppingTheRitual :: CardDef
stoppingTheRitual = act "05127" "Stopping the Ritual" 3 TheSecretName

inPursuitOfTheDead :: CardDef
inPursuitOfTheDead = act "05164" "In Pursuit of the Dead" 1 TheWagesOfSin

inPursuitOfTheLiving :: CardDef
inPursuitOfTheLiving = act "05165" "In Pursuit of the Living" 2 TheWagesOfSin

warmWelcome :: CardDef
warmWelcome = act "05200" "Warm Welcome" 1 ForTheGreaterGood

infiltratingTheLodge :: CardDef
infiltratingTheLodge = act "05201" "Infiltrating the Lodge" 1 ForTheGreaterGood

obtainingTheDevice :: CardDef
obtainingTheDevice = act "05202" "Obtaining the Device" 2 ForTheGreaterGood

theFourKeys :: CardDef
theFourKeys = act "05203" "The Four Keys" 3 ForTheGreaterGood

theUnvisitedIsle :: CardDef
theUnvisitedIsle = act "05241" "The Unvisited Isle" 1 UnionAndDisillusion

fatedSouls :: CardDef
fatedSouls = act "05242" "Fated Souls" 2 UnionAndDisillusion

beyondTheMistV1 :: CardDef
beyondTheMistV1 = act "05243" "Beyond the Mist (v. I)" 3 UnionAndDisillusion

beyondTheMistV2 :: CardDef
beyondTheMistV2 = act "05244" "Beyond the Mist (v. II)" 3 UnionAndDisillusion

beyondTheMistV3 :: CardDef
beyondTheMistV3 = act "05245" "Beyond the Mist (v. III)" 3 UnionAndDisillusion

beyondTheMistV4 :: CardDef
beyondTheMistV4 = act "05246" "Beyond the Mist (v. IV)" 3 UnionAndDisillusion

theBindingRite :: CardDef
theBindingRite = act "05247" "The Binding Rite" 4 UnionAndDisillusion

theBrokenRite :: CardDef
theBrokenRite = act "05248" "The Broken Rite" 4 UnionAndDisillusion

darkKnowledgeV1 :: CardDef
darkKnowledgeV1 = act "05286a" "Dark Knowledge (v. I)" 1 MusicOfTheDamned

beyondTheGrave :: CardDef
beyondTheGrave = act "05287" "Beyond the Grave" 2 MusicOfTheDamned

darkKnowledgeV2 :: CardDef
darkKnowledgeV2 = act "05288a" "Dark Knowledge (v. II)" 1 SecretsOfTheUniverse

newWorldOrder :: CardDef
newWorldOrder = act "05289" "New World Order" 2 SecretsOfTheUniverse

theCosmosBeckons :: CardDef
theCosmosBeckons = act "05329" "The Cosmos Beckons" 1 BeforeTheBlackThrone

inAzathothsDomain :: CardDef
inAzathothsDomain = act "05330" "In Azathoth's Domain" 2 BeforeTheBlackThrone

whatMustBeDone :: CardDef
whatMustBeDone = act "05331" "What Must Be Done" 3 BeforeTheBlackThrone

enteringTheDreamlands :: CardDef
enteringTheDreamlands = act "06041" "Entering the Dreamlands" 1 BeyondTheGatesOfSleep

theTrialOfNashtAndKamanThah :: CardDef
theTrialOfNashtAndKamanThah = act "06042" "The Trial of Nasht and Kaman-Thah" 2 BeyondTheGatesOfSleep

theFinalDescent :: CardDef
theFinalDescent = act "06043" "The Final Descent" 3 BeyondTheGatesOfSleep

thePath :: CardDef
thePath = act "06044" "The Path" 4 BeyondTheGatesOfSleep

lookingForAnswers :: CardDef
lookingForAnswers = act "06067" "Looking for Answers" 1 WakingNightmare

searchForThePatient :: CardDef
searchForThePatient = act "06068" "Search for the Patient" 2 WakingNightmare

containingTheOutbreak :: CardDef
containingTheOutbreak = act "06069" "Containing the Outbreak" 3 WakingNightmare

kingdomOfTheSkai :: CardDef
kingdomOfTheSkai = act "06122" "Kingdom of the Skai" 1 TheSearchForKadath

theIsleOfOriab :: CardDef
theIsleOfOriab = act "06123" "The Isle of Oriab" 2 TheSearchForKadath

theDoomThatCameBefore :: CardDef
theDoomThatCameBefore = act "06124" "The Doom That Came Before" 2 TheSearchForKadath

seekOutTheNight :: CardDef
seekOutTheNight = act "06125" "Seek Out the Night" 2 TheSearchForKadath

theKingsDecree :: CardDef
theKingsDecree = act "06126" "The King's Decree" 2 TheSearchForKadath

searchingTheUnnamable :: CardDef
searchingTheUnnamable = act "06172" "Searching the Unnamable" 1 AThousandShapesOfHorror

theEndlessStairs :: CardDef
theEndlessStairs = act "06173" "The Endless Stairs" 2 AThousandShapesOfHorror

inTheBellyOfTheMoonBeast :: CardDef
inTheBellyOfTheMoonBeast = act "06210" "In the Belly of the Moon-Beast" 1 DarkSideOfTheMoon

exploringTheMoon :: CardDef
exploringTheMoon = act "06211" "Exploring the Moon" 2 DarkSideOfTheMoon

theMoonsCore :: CardDef
theMoonsCore = act "06212" "The Moon's Core" 3 DarkSideOfTheMoon

unexpectedRescue :: CardDef
unexpectedRescue = act "06213" "Unexpected Rescue" 4 DarkSideOfTheMoon

enteringTheUnderworldV1 :: CardDef
enteringTheUnderworldV1 = act "06250" "Entering the Underworld (v. I)" 1 PointOfNoReturn

enteringTheUnderworldV2 :: CardDef
enteringTheUnderworldV2 = act "06251" "Entering the Underworld (v. II)" 1 PointOfNoReturn

theDescent :: CardDef
theDescent = act "06252" "The Descent" 2 PointOfNoReturn

theBlackExpanse :: CardDef
theBlackExpanse = act "06253" "The Black Expanse" 3 PointOfNoReturn

journeyThroughTheColdWastes :: CardDef
journeyThroughTheColdWastes = act "06290" "Journey Through the Cold Wastes" 1 WhereTheGodsDwell

theThingInTheRobes :: CardDef
theThingInTheRobes = act "06291" "The Thing in the Robes" 2 WhereTheGodsDwell

beyondDreams :: CardDef
beyondDreams = act "06292" "Beyond Dreams" 3 WhereTheGodsDwell

truthAndLies :: CardDef
truthAndLies = act "06293" "Truth and Lies" 4 WhereTheGodsDwell

theDreamEaters :: CardDef
theDreamEaters = act "06294" "The Dream-Eaters" 5 WhereTheGodsDwell

journeyAcrossTheBridge :: CardDef
journeyAcrossTheBridge = act "06337" "Journey Across the Bridge" 1 WeaverOfTheCosmos

theWeaverOfTheCosmos :: CardDef
theWeaverOfTheCosmos = act "06338" "The Weaver of the Cosmos" 2 WeaverOfTheCosmos

theSchemesDemise :: CardDef
theSchemesDemise = act "06339" "The Scheme's Demise" 3 WeaverOfTheCosmos

thePit :: CardDef
thePit = act "07045" "The Pit" 1 ThePitOfDespair

theEscape :: CardDef
theEscape = act "07046" "The Escape" 2 ThePitOfDespair

theSearchForAgentHarper :: CardDef
theSearchForAgentHarper = act "07060" "The Search for Agent Harper" 1 TheVanishingOfElinaHarper

theRescue :: CardDef
theRescue = act "07061" "The Rescue" 2 TheVanishingOfElinaHarper

throughTheLabyrinth :: CardDef
throughTheLabyrinth = act "07128" "Through the Labyrinth" 1 InTooDeep

reefOfMysteries :: CardDef
reefOfMysteries = act "07167" "Reef of Mysteries" 1 DevilReef

pedalToTheMetal :: CardDef
pedalToTheMetal = act "07202" "Pedal to the Metal" 1 HorrorInHighGear

theLighthouse :: CardDef
theLighthouse = act "07236" "The Lighthouse" 1 ALightInTheFog

findingThePath :: CardDef
findingThePath = act "07237" "Finding the Path" 2 ALightInTheFog

worshippersOfTheDeep :: CardDef
worshippersOfTheDeep = act "07238" "Worshippers of the Deep" 3 ALightInTheFog

theFirstOath :: CardDef
theFirstOath = act "07280" "The First Oath" 1 TheLairOfDagon

theSecondOath :: CardDef
theSecondOath = act "07281" "The Second Oath" 2 TheLairOfDagon

theThirdOath :: CardDef
theThirdOath = act "07282" "The Third Oath" 3 TheLairOfDagon

backIntoTheDepths :: CardDef
backIntoTheDepths = act "07315" "Back into the Depths" 1 IntoTheMaelstrom

cityOfTheDeepV1 :: CardDef
cityOfTheDeepV1 = (act "07316" "City of the Deep (v.I)" 2 IntoTheMaelstrom) {cdVictoryPoints = Just 2}

cityOfTheDeepV2 :: CardDef
cityOfTheDeepV2 = (act "07317" "City of the Deep (v.II)" 2 IntoTheMaelstrom) {cdVictoryPoints = Just 2}

cityOfTheDeepV3 :: CardDef
cityOfTheDeepV3 = (act "07318" "City of the Deep (v.III)" 2 IntoTheMaelstrom) {cdVictoryPoints = Just 2}

searchForACampSite :: CardDef
searchForACampSite = act "08521" "Search for a Camp Site" 1 TheCrash

theLostExpedition :: CardDef
theLostExpedition = act "08526" "The Lost Expedition" 2 LostInTheNight

underAttack :: CardDef
underAttack = act "08546" "Under Attack!" 3 SeepingNightmares

shadowOfThePastV1 :: CardDef
shadowOfThePastV1 = act "08553" "Shadow of the Past (v. I)" 1 FatalMirage

shadowOfThePastV2 :: CardDef
shadowOfThePastV2 = act "08554" "Shadow of the Past (v. II)" 2 FatalMirage

shadowOfThePastV3 :: CardDef
shadowOfThePastV3 = act "08555" "Shadow of the Past (v. III)" 3 FatalMirage

ascendTheMountain :: CardDef
ascendTheMountain = act "08599" "Ascend the Mountain" 1 ToTheForbiddenPeaks

sprawlingCityV1 :: CardDef
sprawlingCityV1 = act "08624" "Sprawling the City (v. I)" 1 CityOfTheElderThings

sprawlingCityV2 :: CardDef
sprawlingCityV2 = act "08625" "Sprawling the City (v. II)" 1 CityOfTheElderThings

sprawlingCityV3 :: CardDef
sprawlingCityV3 = act "08626" "Sprawling the City (v. II)" 1 CityOfTheElderThings

pursuitOfTheUnknownV1 :: CardDef
pursuitOfTheUnknownV1 = act "08627" "Pursuit of the Unknown (v. I)" 2 CityOfTheElderThings

pursuitOfTheUnknownV2 :: CardDef
pursuitOfTheUnknownV2 = act "08628" "Pursuit of the Unknown (v. II)" 2 CityOfTheElderThings

pursuitOfTheUnknownV3 :: CardDef
pursuitOfTheUnknownV3 = act "08629" "Pursuit of the Unknown (v. III)" 2 CityOfTheElderThings

dormancy :: CardDef
dormancy = act "08662" "Dormancy" 1 TheGreatSeal

theGreatSeal :: CardDef
theGreatSeal = act "08663" "The Great Seal" 2 TheGreatSeal

collapseThePylons :: CardDef
collapseThePylons = act "08673" "Collapse the Pylons" 3 StirringInTheDeep

theFinalMirage :: CardDef
theFinalMirage = act "08681" "The Final Mirage" 4 StirringInTheDeep

mysteriousGateway :: CardDef
mysteriousGateway = act "50012" "Mysterious Gateway" 1 ReturnToTheGathering

intoTheBeyondV2 :: CardDef
intoTheBeyondV2 = act "51055" "Into the Beyond (v. II)" 2 ReturnToLostInTimeAndSpace

theStrangerAlaranMists :: CardDef
theStrangerAlaranMists = act "52015" "The Stranger" 2 ReturnToCurtainCall

theStrangerUnderTheCity :: CardDef
theStrangerUnderTheCity = act "52016" "The Stranger" 2 ReturnToCurtainCall

theStrangerHereIsMyReply :: CardDef
theStrangerHereIsMyReply = act "52017" "The Stranger" 2 ReturnToCurtainCall

theReallyBadOnesV3 :: CardDef
theReallyBadOnesV3 =
  act "52035" "\"The Really Bad Ones\" (v. III)" 2 ReturnToTheUnspeakableOath

theReallyBadOnesV4 :: CardDef
theReallyBadOnesV4 =
  act "52036" "\"The Really Bad Ones\" (v. IV)" 2 ReturnToTheUnspeakableOath

searchForTheMeaning :: CardDef
searchForTheMeaning = act "53029" "Search for the Meaning" 1 ReturnToThreadsOfFate

searchForTheSource :: CardDef
searchForTheSource = act "53030" "Search for the Source" 1 ReturnToThreadsOfFate

theOvergrownEstateSentFromAnotherTime :: CardDef
theOvergrownEstateSentFromAnotherTime = act "53031" "The Overgrown Estate" 2 ReturnToThreadsOfFate

theOvergrownEstateClintonFreeman :: CardDef
theOvergrownEstateClintonFreeman = act "53032" "The Overgrown Estate" 2 ReturnToThreadsOfFate

seekingTroubleSentFromAnotherTime :: CardDef
seekingTroubleSentFromAnotherTime = act "53033" "Seeking Trouble" 2 ReturnToThreadsOfFate

seekingTroubleLoadingDocks :: CardDef
seekingTroubleLoadingDocks = act "53034" "Seeking Trouble" 2 ReturnToThreadsOfFate

discoverTheTruth :: CardDef
discoverTheTruth = act "53035" "Discover the Truth" 3 ReturnToThreadsOfFate

impossiblePursuit :: CardDef
impossiblePursuit = act "53036" "Impossible Pursuit" 3 ReturnToThreadsOfFate

aFamiliarPattern :: CardDef
aFamiliarPattern = act "53046" "A Familiar Pattern" 1 ReturnToPillarsOfJudgement

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

whatHappened :: CardDef
whatHappened = act "84004" "What Happened...?" 1 MurderAtTheExcelsiorHotel

followingLeads :: CardDef
followingLeads = act "84005" "Following Leads" 2 MurderAtTheExcelsiorHotel

meetAndGreet :: CardDef
meetAndGreet = act "71005" "Meet and Greet" 1 TheMidwinterGala

findingTheJewel :: CardDef
findingTheJewel = act "71006" "Finding the Jewel" 2 TheMidwinterGala
