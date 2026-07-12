module Arkham.Treachery.CardDefs.CircusExMortis where

import Arkham.Treachery.CardDefs.Import

-- one_night_only
maddeningSpectacleCircusExMortis :: CardDef
maddeningSpectacleCircusExMortis =
  (treachery "z-circus-ex-mortis-015" "Maddening Spectacle" CircusExMortisOneNightOnly 3) {cdCardTraits = setFromList [Hex]}

-- the_primrose_path
feralImpulsesCircusExMortis :: CardDef
feralImpulsesCircusExMortis =
  (treachery "z-circus-ex-mortis-039" "Feral Impulses" CircusExMortisThePrimrosePath 3) {cdCardTraits = setFromList [Madness]}

moonlightIllusionCircusExMortis :: CardDef
moonlightIllusionCircusExMortis =
  (treachery "z-circus-ex-mortis-040" "Moonlight Illusion" CircusExMortisThePrimrosePath 3) {cdCardTraits = setFromList [Hex]}

-- harm_s_way
closeWatchCircusExMortis :: CardDef
closeWatchCircusExMortis =
  (treachery "z-circus-ex-mortis-072" "Close Watch" CircusExMortisHarmsWay 2) {cdCardTraits = setFromList [Scheme]}

keepQuietCircusExMortis :: CardDef
keepQuietCircusExMortis = (treachery "z-circus-ex-mortis-073" "Keep Quiet" CircusExMortisHarmsWay 2)

violentThrashingCircusExMortis :: CardDef
violentThrashingCircusExMortis =
  (treachery "z-circus-ex-mortis-075" "Violent Thrashing" CircusExMortisHarmsWay 3) {cdCardTraits = setFromList [Attack]}

-- all_points_west
brokenCouplingsCircusExMortis :: CardDef
brokenCouplingsCircusExMortis =
  (treachery "z-circus-ex-mortis-104" "Broken Couplings" CircusExMortisAllPointsWest 2) {cdCardTraits = setFromList [Obstacle]}

focusedSabotageCircusExMortis :: CardDef
focusedSabotageCircusExMortis =
  (treachery "z-circus-ex-mortis-105" "Focused Sabotage" CircusExMortisAllPointsWest 2) {cdCardTraits = setFromList [Power, Scheme]}

overloadedEngineCircusExMortis :: CardDef
overloadedEngineCircusExMortis =
  (treachery "z-circus-ex-mortis-108" "Overloaded Engine" CircusExMortisAllPointsWest 3) {cdCardTraits = setFromList [Hazard]}

ricketyRideCircusExMortis :: CardDef
ricketyRideCircusExMortis =
  (treachery "z-circus-ex-mortis-109" "Rickety Ride" CircusExMortisAllPointsWest 3) {cdCardTraits = setFromList [Hazard]}

-- piper_at_the_gates_of_dawn
duplicitousIllusionCircusExMortis :: CardDef
duplicitousIllusionCircusExMortis =
  (treachery "z-circus-ex-mortis-121" "Duplicitous Illusion" CircusExMortisPiperAtTheGatesOfDawn 3) {cdCardTraits = setFromList [Hex, Paradox]}

phantomBeastsCircusExMortis :: CardDef
phantomBeastsCircusExMortis =
  (treachery "z-circus-ex-mortis-122" "Phantom Beasts" CircusExMortisPiperAtTheGatesOfDawn 3) {cdCardTraits = setFromList [Hex, Attack]}

shadowyPerformanceCircusExMortis :: CardDef
shadowyPerformanceCircusExMortis =
  (treachery "z-circus-ex-mortis-123" "Shadowy Performance" CircusExMortisPiperAtTheGatesOfDawn 3) {cdCardTraits = setFromList [Hex, Scheme]}

-- bacchanalia
allThatGlittersCircusExMortis :: CardDef
allThatGlittersCircusExMortis =
  (treachery "z-circus-ex-mortis-146" "All That Glitters" CircusExMortisBacchanalia 2) {cdCardTraits = setFromList [Scheme, Flaw]}

bestLeftUnsaidCircusExMortis :: CardDef
bestLeftUnsaidCircusExMortis =
  (treachery "z-circus-ex-mortis-147" "Best Left Unsaid" CircusExMortisBacchanalia 2) {cdCardTraits = setFromList [Scheme]}

destructiveImpulsesCircusExMortis :: CardDef
destructiveImpulsesCircusExMortis =
  (treachery "z-circus-ex-mortis-149" "Destructive Impulses" CircusExMortisBacchanalia 2) {cdCardTraits = setFromList [Scheme, Flaw]}

drinkAndBeMerryCircusExMortis :: CardDef
drinkAndBeMerryCircusExMortis =
  (treachery "z-circus-ex-mortis-150" "Drink and Be Merry" CircusExMortisBacchanalia 2) {cdCardTraits = setFromList [Scheme, Flaw]}

perfumeAndPassionCircusExMortis :: CardDef
perfumeAndPassionCircusExMortis =
  (treachery "z-circus-ex-mortis-152" "Perfume and Passion" CircusExMortisBacchanalia 2) {cdCardTraits = setFromList [Scheme, Flaw]}

-- red_sunrise
crashingTreesCircusExMortis :: CardDef
crashingTreesCircusExMortis =
  (treachery "z-circus-ex-mortis-188" "Crashing Trees" CircusExMortisRedSunrise 2) {cdCardTraits = setFromList [Hazard]}

silentForestCircusExMortis :: CardDef
silentForestCircusExMortis =
  (treachery "z-circus-ex-mortis-191" "Silent Forest" CircusExMortisRedSunrise 2) {cdCardTraits = setFromList [Terror]}

-- thousand_to_one
balefulEclipseCircusExMortis :: CardDef
balefulEclipseCircusExMortis =
  (treachery "z-circus-ex-mortis-210" "Baleful Eclipse" CircusExMortisThousandToOne 2) {cdCardTraits = setFromList [Hex, Omen]}

dreadOfTheNewMoonCircusExMortis :: CardDef
dreadOfTheNewMoonCircusExMortis =
  (treachery "z-circus-ex-mortis-212" "Dread of the New Moon" CircusExMortisThousandToOne 3) {cdCardTraits = setFromList [Terror, Madness]}

hungerOfThousandsCircusExMortis :: CardDef
hungerOfThousandsCircusExMortis =
  (treachery "z-circus-ex-mortis-213" "Hunger of Thousands" CircusExMortisThousandToOne 2) {cdCardTraits = setFromList [Attack]}

ireOfShubNiggurathCircusExMortis :: CardDef
ireOfShubNiggurathCircusExMortis =
  (treachery "z-circus-ex-mortis-214" "Ire of Shub-Niggurath" CircusExMortisThousandToOne 2) {cdCardTraits = setFromList [Terror, Curse]}

-- children_of_the_goat
endlessSpawnCircusExMortis :: CardDef
endlessSpawnCircusExMortis =
  (treachery "z-circus-ex-mortis-216" "Endless Spawn" CircusExMortisChildrenOfTheGoat 2) {cdCardTraits = setFromList [Power]}

-- cult_of_shub-niggurath
milkOfShubNiggurathCircusExMortis :: CardDef
milkOfShubNiggurathCircusExMortis =
  (treachery "z-circus-ex-mortis-226" "Milk of Shub-Niggurath" CircusExMortisCultOfShubNiggurath 2) {cdCardTraits = setFromList [Power]}

-- illusory_tricks
hypnoticGlamourCircusExMortis :: CardDef
hypnoticGlamourCircusExMortis =
  (treachery "z-circus-ex-mortis-243" "Hypnotic Glamour" CircusExMortisIllusoryTricks 2) {cdCardTraits = setFromList [Hex]}

phantasmalDeceptionCircusExMortis :: CardDef
phantasmalDeceptionCircusExMortis =
  (treachery "z-circus-ex-mortis-244" "Phantasmal Deception" CircusExMortisIllusoryTricks 2) {cdCardTraits = setFromList [Scheme, Hex]}

-- lunatic_night
lunarInfluenceCircusExMortis :: CardDef
lunarInfluenceCircusExMortis =
  (treachery "z-circus-ex-mortis-245" "Lunar Influence" CircusExMortisLunaticNight 3) {cdCardTraits = setFromList [Mystery, Terror]}

ominousMoonlightCircusExMortis :: CardDef
ominousMoonlightCircusExMortis =
  (treachery "z-circus-ex-mortis-247" "Ominous Moonlight" CircusExMortisLunaticNight 3) {cdCardTraits = setFromList [Mystery, Omen]}

-- new_moon_daredevils
recklessStuntCircusExMortis :: CardDef
recklessStuntCircusExMortis =
  (treachery "z-circus-ex-mortis-251" "Reckless Stunt" CircusExMortisNewMoonDaredevils 2) {cdCardTraits = setFromList [Scheme, Hazard]}

-- new_moon_entertainers
quickerThanTheEyeCircusExMortis :: CardDef
quickerThanTheEyeCircusExMortis =
  (treachery "z-circus-ex-mortis-255" "Quicker Than the Eye" CircusExMortisNewMoonEntertainers 2) {cdCardTraits = setFromList [Scheme]}

-- panicked_masses
lostAllControlCircusExMortis :: CardDef
lostAllControlCircusExMortis =
  (treachery "z-circus-ex-mortis-257" "Lost All Control" CircusExMortisPanickedMasses 2) {cdCardTraits = setFromList [Madness]}

wildHysteriaCircusExMortis :: CardDef
wildHysteriaCircusExMortis =
  (treachery "z-circus-ex-mortis-258" "Wild Hysteria" CircusExMortisPanickedMasses 2) {cdCardTraits = setFromList [Terror, Blunder]}

-- primordial_evils
primordialEvilsCircusExMortis :: CardDef
primordialEvilsCircusExMortis =
  (treachery "z-circus-ex-mortis-259" "Primordial Evils" CircusExMortisPrimordialEvils 3) {cdCardTraits = setFromList [Omen]}

-- savage_woods
denseTangleCircusExMortis :: CardDef
denseTangleCircusExMortis =
  (treachery "z-circus-ex-mortis-260" "Dense Tangle" CircusExMortisSavageWoods 2) {cdCardTraits = setFromList [Obstacle]}

lostTheTrailCircusExMortis :: CardDef
lostTheTrailCircusExMortis =
  (treachery "z-circus-ex-mortis-261" "Lost the Trail" CircusExMortisSavageWoods 2) {cdCardTraits = setFromList [Blunder]}
