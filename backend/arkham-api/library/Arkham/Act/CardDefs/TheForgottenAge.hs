module Arkham.Act.CardDefs.TheForgottenAge where

import Arkham.Act.CardDefs.Import

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
