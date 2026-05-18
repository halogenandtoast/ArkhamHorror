module Arkham.Treachery.CardDefs.TheForgottenAge where

import Arkham.Treachery.CardDefs.Import
import Arkham.Keyword qualified as Keyword
import Arkham.EncounterSet qualified as EncounterSet

boughtInBlood :: CardDef
boughtInBlood =
  (weakness "04007" "Bought in Blood") {cdCardTraits = singleton Flaw}

callOfTheUnknown :: CardDef
callOfTheUnknown =
  (weakness "04009" "Call of the Unknown") {cdCardTraits = singleton Task}

caughtRedHanded :: CardDef
caughtRedHanded =
  (weakness "04012" "Caught Red-Handed") {cdCardTraits = singleton Blunder}

voiceOfTheMessenger :: CardDef
voiceOfTheMessenger =
  (weakness "04016" "Voice of the Messenger")
    { cdCardTraits = setFromList [Curse, Pact]
    }

thePriceOfFailure :: CardDef
thePriceOfFailure =
  (weakness "04039" "The Price of Failure") {cdCardTraits = singleton Pact}

doomed :: CardDef
doomed =
  (basicWeakness "04040" "Doomed")
    { cdCardTraits = singleton Curse
    , cdDeckRestrictions = [CampaignModeOnly]
    }

accursedFate :: CardDef
accursedFate =
  (weakness "04041" "Accursed Fate") {cdCardTraits = singleton Curse}

theBellTolls :: CardDef
theBellTolls =
  (weakness "04042" "The Bell Tolls") {cdCardTraits = singleton Curse}

overgrowth :: CardDef
overgrowth =
  (treachery "04076" "Overgrowth" Rainforest 2)
    { cdCardTraits = singleton Obstacle
    }

voiceOfTheJungle :: CardDef
voiceOfTheJungle =
  (treachery "04077" "Voice of the Jungle" Rainforest 2)
    { cdCardTraits = singleton Power
    }

snakeBite :: CardDef
snakeBite =
  (treachery "04080" "Snake Bite" Serpents 3)
    { cdCardTraits = setFromList [Hazard, Poison]
    }

lostInTheWilds :: CardDef
lostInTheWilds =
  (treachery "04081" "Lost in the Wilds" Expedition 3)
    { cdCardTraits = singleton Blunder
    }

lowOnSupplies :: CardDef
lowOnSupplies =
  (treachery "04082" "Low on Supplies" Expedition 2)
    { cdCardTraits = singleton Blunder
    , cdKeywords = singleton Keyword.Peril
    }

curseOfYig :: CardDef
curseOfYig =
  (treachery "04085" "Curse of Yig" AgentsOfYig 2)
    { cdCardTraits = singleton Curse
    }

arrowsFromTheTrees :: CardDef
arrowsFromTheTrees =
  (treachery "04087" "Arrows from the Trees" GuardiansOfTime 2)
    { cdCardTraits = singleton Scheme
    }

finalMistake :: CardDef
finalMistake =
  (treachery "04088" "Final Mistake" DeadlyTraps 3)
    { cdCardTraits = singleton Trap
    }

entombed :: CardDef
entombed =
  (treachery "04089" "Entombed" DeadlyTraps 2)
    { cdCardTraits = singleton Trap
    }

aTearInTime :: CardDef
aTearInTime =
  (treachery "04090" "A Tear in Time" TemporalFlux 3)
    { cdCardTraits = singleton Hex
    }

lostInTime :: CardDef
lostInTime =
  (treachery "04091" "Lost in Time" TemporalFlux 2)
    { cdCardTraits = singleton Hex
    }

illOmen :: CardDef
illOmen =
  (treachery "04092" "Ill Omen" ForgottenRuins 2)
    { cdCardTraits = setFromList [Omen, Terror]
    , cdKeywords = singleton Keyword.Peril
    }

ancestralFear :: CardDef
ancestralFear =
  (treachery "04093" "Ancestral Fear" ForgottenRuins 2)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Surge]
    , cdVengeancePoints = Just 1
    }

deepDark :: CardDef
deepDark =
  (treachery "04094" "Deep Dark" ForgottenRuins 3)
    { cdCardTraits = singleton Hazard
    }

shadowed :: CardDef
shadowed =
  (treachery "04096" "Shadowed" PnakoticBrotherhood 2)
    { cdCardTraits = singleton Scheme
    }

wordsOfPower :: CardDef
wordsOfPower =
  (treachery "04097" "Words of Power" PnakoticBrotherhood 2)
    { cdCardTraits = singleton Hex
    }

snakescourge :: CardDef
snakescourge =
  (treachery "04099" "Snakescourge" YigsVenom 2)
    { cdCardTraits = singleton Curse
    }

serpentsCall :: CardDef
serpentsCall =
  (treachery "04100" "Serpent's Call" YigsVenom 1)
    { cdCardTraits = singleton Power
    }

creepingPoison :: CardDef
creepingPoison =
  (treachery "04101" "Creeping Poison" EncounterSet.Poison 2)
    { cdCardTraits = singleton Poison
    , cdKeywords = singleton Keyword.Surge
    }

poisoned :: CardDef
poisoned =
  (weakness "04102" "Poisoned")
    { cdCardTraits = singleton Poison
    , cdPermanent = True
    , cdEncounterSet = Just EncounterSet.Poison
    , cdEncounterSetQuantity = Just 4
    }

theSecretMustBeKept :: CardDef
theSecretMustBeKept =
  (treachery "04144" "The Secret Must Be Kept" EncounterSet.ThreadsOfFate 3)
    { cdCardTraits = singleton Scheme
    , cdKeywords = singleton Keyword.Peril
    }

nobodysHome :: CardDef
nobodysHome =
  (treachery "04145" "Nobody's Home" EncounterSet.ThreadsOfFate 2)
    { cdCardTraits = singleton Mystery
    }

conspiracyOfBlood :: CardDef
conspiracyOfBlood =
  (treachery "04146" "Conspiracy of Blood" EncounterSet.ThreadsOfFate 2)
    { cdCardTraits = singleton Hex
    }

windowToAnotherTime :: CardDef
windowToAnotherTime =
  (treachery "04189" "Window to Another Time" EncounterSet.TheBoundaryBeyond 3)
    { cdCardTraits = singleton Hex
    , cdKeywords = singleton Keyword.Peril
    }

timelineDestabilization :: CardDef
timelineDestabilization =
  (treachery "04190" "Timeline Destabilization" EncounterSet.TheBoundaryBeyond 3)
    { cdCardTraits = singleton Hex
    }

pitfall :: CardDef
pitfall =
  (treachery "04215" "Pitfall" EncounterSet.HeartOfTheElders 3)
    { cdCardTraits = singleton Trap
    , cdKeywords = singleton Keyword.Peril
    }

poisonousSpores :: CardDef
poisonousSpores =
  (treachery "04216" "Poisonous Spores" EncounterSet.HeartOfTheElders 3)
    { cdCardTraits = singleton Hazard
    }

ants :: CardDef
ants =
  (treachery "04221" "Ants!" EncounterSet.PillarsOfJudgement 3)
    { cdCardTraits = singleton Hazard
    }

noTurningBack :: CardDef
noTurningBack =
  (treachery "04228" "No Turning Back" EncounterSet.KnYan 3)
    { cdCardTraits = singleton Hazard
    }

yithianPresence :: CardDef
yithianPresence =
  (treachery "04260" "Yithian Presence" EncounterSet.TheCityOfArchives 3)
    { cdCardTraits = setFromList [Power, Terror]
    }

cruelInterrogations :: CardDef
cruelInterrogations =
  (treachery "04261" "Cruel Interrogations" EncounterSet.TheCityOfArchives 3)
    { cdCardTraits = setFromList [Injury, Terror]
    }

lostHumanity :: CardDef
lostHumanity =
  (treachery "04262" "Lost Humanity" EncounterSet.TheCityOfArchives 2)
    { cdCardTraits = singleton Terror
    }

captiveMind :: CardDef
captiveMind =
  (treachery "04263" "Captive Mind" EncounterSet.TheCityOfArchives 2)
    { cdCardTraits = singleton Hex
    }

outOfBodyExperience :: CardDef
outOfBodyExperience =
  (weakness "04264" "Out of Body Experience")
    { cdCardTraits = setFromList [Madness, Paradox]
    , cdEncounterSet = Just TheCityOfArchives
    , cdEncounterSetQuantity = Just 4
    }

childrenOfValusia :: CardDef
childrenOfValusia =
  (treachery "04299" "Children of Valusia" TheDepthsOfYoth 3)
    { cdCardTraits = singleton Scheme
    }

lightlessShadow :: CardDef
lightlessShadow =
  (treachery "04300" "Lightless Shadow" TheDepthsOfYoth 3)
    { cdCardTraits = singleton Terror
    }

bathophobia :: CardDef
bathophobia =
  (treachery "04301" "Bathophobia" TheDepthsOfYoth 3)
    { cdCardTraits = singleton Terror
    }

serpentsIre :: CardDef
serpentsIre =
  (treachery "04302" "Serpent's Ire" TheDepthsOfYoth 2)
    { cdCardTraits = singleton Scheme
    }

shatteredAges :: CardDef
shatteredAges =
  (treachery "04339" "Shattered Ages" ShatteredAeons 2)
    { cdCardTraits = singleton Hex
    }

betweenWorlds :: CardDef
betweenWorlds =
  (treachery "04340" "Between Worlds" ShatteredAeons 2)
    { cdCardTraits = singleton Hex
    }

wrackedByTime :: CardDef
wrackedByTime =
  (treachery "04341" "Wracked by Time" ShatteredAeons 3)
    { cdCardTraits = singleton Hex
    }

creepingDarkness :: CardDef
creepingDarkness =
  (treachery "04342" "Creeping Darkness" ShatteredAeons 2)
    { cdCardTraits = singleton Hazard
    }
