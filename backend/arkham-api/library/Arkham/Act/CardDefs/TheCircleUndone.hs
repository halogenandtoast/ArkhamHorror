module Arkham.Act.CardDefs.TheCircleUndone where

import Arkham.Act.CardDefs.Import

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
darkKnowledgeV1 = (act "05286a" "Dark Knowledge (v. I)" 1 MusicOfTheDamned) {cdOtherSide = Just "05286b"}

beyondTheGrave :: CardDef
beyondTheGrave = act "05287" "Beyond the Grave" 2 MusicOfTheDamned

darkKnowledgeV2 :: CardDef
darkKnowledgeV2 = (act "05288a" "Dark Knowledge (v. II)" 1 SecretsOfTheUniverse) {cdOtherSide = Just "05288b"}

newWorldOrder :: CardDef
newWorldOrder = act "05289" "New World Order" 2 SecretsOfTheUniverse

theCosmosBeckons :: CardDef
theCosmosBeckons = act "05329" "The Cosmos Beckons" 1 BeforeTheBlackThrone

inAzathothsDomain :: CardDef
inAzathothsDomain = act "05330" "In Azathoth's Domain" 2 BeforeTheBlackThrone

whatMustBeDone :: CardDef
whatMustBeDone = act "05331" "What Must Be Done" 3 BeforeTheBlackThrone
