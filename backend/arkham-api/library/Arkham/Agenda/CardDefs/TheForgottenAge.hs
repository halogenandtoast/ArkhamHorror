module Arkham.Agenda.CardDefs.TheForgottenAge where

import Arkham.Agenda.CardDefs.Import

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
