module Arkham.Treachery.CardDefs.TheDunwichLegacy where

import Arkham.Treachery.CardDefs.Import
import Arkham.Keyword qualified as Keyword
import Arkham.EncounterSet qualified as EncounterSet

smiteTheWicked :: CardDef
smiteTheWicked =
  (weakness "02007" "Smite the Wicked") {cdCardTraits = setFromList [Task]}

rexsCurse :: CardDef
rexsCurse =
  (weakness "02009" "Rex's Curse") {cdCardTraits = setFromList [Curse]}

searchingForIzzie :: CardDef
searchingForIzzie =
  (weakness "02011" "Searching for Izzie") {cdCardTraits = setFromList [Task]}

finalRhapsody :: CardDef
finalRhapsody =
  (weakness "02013" "Final Rhapsody") {cdCardTraits = setFromList [Endtimes]}

wrackedByNightmares :: CardDef
wrackedByNightmares =
  (weakness "02015" "Wracked by Nightmares")
    { cdCardTraits = setFromList [Madness]
    }

indebted :: CardDef
indebted =
  (basicWeakness "02037" "Indebted")
    { cdCardTraits = singleton Flaw
    , cdPermanent = True
    }

internalInjury :: CardDef
internalInjury =
  (basicWeakness "02038" "Internal Injury") {cdCardTraits = singleton Injury}

chronophobia :: CardDef
chronophobia =
  (basicWeakness "02039" "Chronophobia") {cdCardTraits = singleton Madness}

somethingInTheDrinks :: CardDef
somethingInTheDrinks =
  (treachery "02081" "Something in the Drinks" TheHouseAlwaysWins 2)
    { cdCardTraits = setFromList [Poison, Illicit]
    , cdKeywords = setFromList [Keyword.Surge]
    }

arousingSuspicions :: CardDef
arousingSuspicions =
  treachery "02082" "Arousing Suspicions" TheHouseAlwaysWins 2

visionsOfFuturesPast :: CardDef
visionsOfFuturesPast =
  (treachery "02083" "Visions of Futures Past" Sorcery 3)
    { cdCardTraits = setFromList [Hex]
    }

beyondTheVeil :: CardDef
beyondTheVeil =
  (treachery "02084" "Beyond the Veil" Sorcery 3)
    { cdCardTraits = setFromList [Hex]
    , cdKeywords = setFromList [Keyword.Surge]
    }

lightOfAforgomon :: CardDef
lightOfAforgomon =
  (treachery "02085" "Light of Aforgomon" BishopsThralls 2)
    { cdCardTraits = setFromList [Pact, Power]
    , cdKeywords = setFromList [Keyword.Peril]
    }

unhallowedCountry :: CardDef
unhallowedCountry =
  (treachery "02088" "Unhallowed Country" EncounterSet.Dunwich 2)
    { cdCardTraits = setFromList [Terror]
    }

sordidAndSilent :: CardDef
sordidAndSilent =
  (treachery "02089" "Sordid and Silent" EncounterSet.Dunwich 2)
    { cdCardTraits = setFromList [Terror]
    }

eagerForDeath :: CardDef
eagerForDeath =
  (treachery "02091" "Eager for Death" Whippoorwills 2)
    { cdCardTraits = setFromList [Omen]
    }

cursedLuck :: CardDef
cursedLuck =
  (treachery "02092" "Cursed Luck" BadLuck 3)
    { cdCardTraits = setFromList [Omen]
    }

twistOfFate :: CardDef
twistOfFate =
  (treachery "02093" "Twist of Fate" BadLuck 3)
    { cdCardTraits = setFromList [Omen]
    }

alteredBeast :: CardDef
alteredBeast =
  (treachery "02096" "Altered Beast" BeastThralls 2)
    { cdCardTraits = setFromList [Power]
    }

huntedDown :: CardDef
huntedDown =
  (treachery "02099" "Hunted Down" NaomisCrew 2)
    { cdCardTraits = setFromList [Tactic]
    }

pushedIntoTheBeyond :: CardDef
pushedIntoTheBeyond =
  (treachery "02100" "Pushed into the Beyond" TheBeyond 2)
    { cdCardTraits = setFromList [Hex]
    }

terrorFromBeyond :: CardDef
terrorFromBeyond =
  (treachery "02101" "Terror from Beyond" TheBeyond 2)
    { cdCardTraits = setFromList [Hex, Terror]
    , cdKeywords = setFromList [Keyword.Peril]
    }

arcaneBarrier :: CardDef
arcaneBarrier =
  (treachery "02102" "Arcane Barrier" TheBeyond 2)
    { cdCardTraits = setFromList [Hex, Obstacle]
    }

shadowSpawned :: CardDef
shadowSpawned =
  (treachery "02142" "Shadow-spawned" TheMiskatonicMuseum 1)
    { cdCardTraits = singleton Power
    }

stalkedInTheDark :: CardDef
stalkedInTheDark =
  (treachery "02143" "Stalked in the Dark" TheMiskatonicMuseum 2)
    { cdCardTraits = singleton Tactic
    }

passageIntoTheVeil :: CardDef
passageIntoTheVeil =
  (treachery "02144" "Passage into the Veil" TheMiskatonicMuseum 3)
    { cdCardTraits = singleton Power
    }

ephemeralExhibits :: CardDef
ephemeralExhibits =
  (treachery "02145" "Ephemeral Exhibits" TheMiskatonicMuseum 2)
    { cdCardTraits = singleton Terror
    }

slitheringBehindYou :: CardDef
slitheringBehindYou =
  treachery "02146" "Slithering Behind You" TheMiskatonicMuseum 2

acrossSpaceAndTime :: CardDef
acrossSpaceAndTime =
  (weakness "02178" "Across Space and Time")
    { cdCardTraits = setFromList [Madness]
    , cdEncounterSet = Just TheEssexCountyExpress
    , cdEncounterSetQuantity = Just 4
    }

clawsOfSteam :: CardDef
clawsOfSteam =
  (treachery "02180" "Claws of Steam" TheEssexCountyExpress 3)
    { cdCardTraits = singleton Power
    }

brokenRails :: CardDef
brokenRails =
  (treachery "02181" "Broken Rails" TheEssexCountyExpress 3)
    { cdCardTraits = singleton Hazard
    }

kidnapped :: CardDef
kidnapped = treachery "02220" "Kidnapped!" BloodOnTheAltar 3

psychopompsSong :: CardDef
psychopompsSong =
  (treachery "02221" "Psychopomp's Song" BloodOnTheAltar 2)
    { cdCardTraits = singleton Omen
    , cdKeywords = setFromList [Keyword.Surge, Keyword.Peril]
    }

strangeSigns :: CardDef
strangeSigns =
  (treachery "02222" "Strange Signs" BloodOnTheAltar 2)
    { cdCardTraits = singleton Omen
    }

rottingRemainsBloodOnTheAltar :: CardDef
rottingRemainsBloodOnTheAltar =
  (treachery "02223" "Rotting Remains" BloodOnTheAltar 3)
    { cdCardTraits = singleton Terror
    }

toweringBeasts :: CardDef
toweringBeasts =
  (treachery "02256" "Towering Beasts" UndimensionedAndUnseen 4)
    { cdKeywords = singleton Keyword.Peril
    }

ruinAndDestruction :: CardDef
ruinAndDestruction =
  (treachery "02257" "Ruin and Destruction" UndimensionedAndUnseen 3)
    { cdCardTraits = singleton Hazard
    }

attractingAttention :: CardDef
attractingAttention =
  (treachery "02258" "Attracting Attention" UndimensionedAndUnseen 2)
    { cdKeywords = singleton Keyword.Surge
    }

theCreaturesTracks :: CardDef
theCreaturesTracks =
  (treachery "02259" "The Creatures' Tracks" UndimensionedAndUnseen 2)
    { cdCardTraits = singleton Terror
    , cdKeywords = singleton Keyword.Peril
    }

ritesHowled :: CardDef
ritesHowled =
  (treachery "02296" "Rites Howled" WhereDoomAwaits 3)
    { cdCardTraits = singleton Hex
    }

spacesBetween :: CardDef
spacesBetween =
  (treachery "02297" "Spaces Between" WhereDoomAwaits 3)
    { cdCardTraits = setFromList [Hex, Hazard]
    }

vortexOfTime :: CardDef
vortexOfTime =
  (treachery "02298" "Vortex of Time" WhereDoomAwaits 3)
    { cdCardTraits = setFromList [Hex, Hazard]
    }

collapsingReality :: CardDef
collapsingReality =
  (treachery "02331" "Collapsing Reality" LostInTimeAndSpace 3)
    { cdCardTraits = setFromList [Hazard]
    }

wormhole :: CardDef
wormhole =
  (treachery "02332" "Wormhole" LostInTimeAndSpace 2)
    { cdCardTraits = setFromList [Hazard]
    }

vastExpanse :: CardDef
vastExpanse =
  (treachery "02333" "Vast Expanse" LostInTimeAndSpace 3)
    { cdCardTraits = setFromList [Terror]
    }
