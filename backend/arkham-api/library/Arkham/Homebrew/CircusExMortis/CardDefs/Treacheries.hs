module Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries where

import Arkham.Treachery.CardDefs.Import
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set

-- one_night_only
maddeningSpectacle :: CardDef
maddeningSpectacle =
  (treachery "z-circus-ex-mortis-015" "Maddening Spectacle" Set.OneNightOnly 3) {cdCardTraits = setFromList [Hex]}

-- the_primrose_path
feralImpulses :: CardDef
feralImpulses =
  (treachery "z-circus-ex-mortis-039" "Feral Impulses" Set.ThePrimrosePath 3) {cdCardTraits = setFromList [Madness]}

moonlightIllusion :: CardDef
moonlightIllusion =
  (treachery "z-circus-ex-mortis-040" "Moonlight Illusion" Set.ThePrimrosePath 3) {cdCardTraits = setFromList [Hex]}

-- harm_s_way
closeWatch :: CardDef
closeWatch =
  (treachery "z-circus-ex-mortis-072" "Close Watch" Set.HarmsWay 2) {cdCardTraits = setFromList [Scheme]}

keepQuiet :: CardDef
keepQuiet = (treachery "z-circus-ex-mortis-073" "Keep Quiet" Set.HarmsWay 2)

violentThrashing :: CardDef
violentThrashing =
  (treachery "z-circus-ex-mortis-075" "Violent Thrashing" Set.HarmsWay 3) {cdCardTraits = setFromList [Attack]}

-- all_points_west
brokenCouplings :: CardDef
brokenCouplings =
  (treachery "z-circus-ex-mortis-104" "Broken Couplings" Set.AllPointsWest 2) {cdCardTraits = setFromList [Obstacle]}

focusedSabotage :: CardDef
focusedSabotage =
  (treachery "z-circus-ex-mortis-105" "Focused Sabotage" Set.AllPointsWest 2) {cdCardTraits = setFromList [Power, Scheme]}

overloadedEngine :: CardDef
overloadedEngine =
  (treachery "z-circus-ex-mortis-108" "Overloaded Engine" Set.AllPointsWest 3) {cdCardTraits = setFromList [Hazard]}

ricketyRide :: CardDef
ricketyRide =
  (treachery "z-circus-ex-mortis-109" "Rickety Ride" Set.AllPointsWest 3) {cdCardTraits = setFromList [Hazard]}

-- piper_at_the_gates_of_dawn
duplicitousIllusion :: CardDef
duplicitousIllusion =
  (treachery "z-circus-ex-mortis-121" "Duplicitous Illusion" Set.PiperAtTheGatesOfDawn 3) {cdCardTraits = setFromList [Hex, Paradox]}

phantomBeasts :: CardDef
phantomBeasts =
  (treachery "z-circus-ex-mortis-122" "Phantom Beasts" Set.PiperAtTheGatesOfDawn 3) {cdCardTraits = setFromList [Hex, Attack]}

shadowyPerformance :: CardDef
shadowyPerformance =
  (treachery "z-circus-ex-mortis-123" "Shadowy Performance" Set.PiperAtTheGatesOfDawn 3) {cdCardTraits = setFromList [Hex, Scheme]}

-- bacchanalia
allThatGlitters :: CardDef
allThatGlitters =
  (treachery "z-circus-ex-mortis-146" "All That Glitters" Set.Bacchanalia 2) {cdCardTraits = setFromList [Scheme, Flaw]}

bestLeftUnsaid :: CardDef
bestLeftUnsaid =
  (treachery "z-circus-ex-mortis-147" "Best Left Unsaid" Set.Bacchanalia 2) {cdCardTraits = setFromList [Scheme]}

destructiveImpulses :: CardDef
destructiveImpulses =
  (treachery "z-circus-ex-mortis-149" "Destructive Impulses" Set.Bacchanalia 2) {cdCardTraits = setFromList [Scheme, Flaw]}

drinkAndBeMerry :: CardDef
drinkAndBeMerry =
  (treachery "z-circus-ex-mortis-150" "Drink and Be Merry" Set.Bacchanalia 2) {cdCardTraits = setFromList [Scheme, Flaw]}

perfumeAndPassion :: CardDef
perfumeAndPassion =
  (treachery "z-circus-ex-mortis-152" "Perfume and Passion" Set.Bacchanalia 2) {cdCardTraits = setFromList [Scheme, Flaw]}

-- red_sunrise
crashingTrees :: CardDef
crashingTrees =
  (treachery "z-circus-ex-mortis-188" "Crashing Trees" Set.RedSunrise 2) {cdCardTraits = setFromList [Hazard]}

silentForest :: CardDef
silentForest =
  (treachery "z-circus-ex-mortis-191" "Silent Forest" Set.RedSunrise 2) {cdCardTraits = setFromList [Terror]}

-- thousand_to_one
balefulEclipse :: CardDef
balefulEclipse =
  (treachery "z-circus-ex-mortis-210" "Baleful Eclipse" Set.ThousandToOne 2) {cdCardTraits = setFromList [Hex, Omen]}

dreadOfTheNewMoon :: CardDef
dreadOfTheNewMoon =
  (treachery "z-circus-ex-mortis-212" "Dread of the New Moon" Set.ThousandToOne 3) {cdCardTraits = setFromList [Terror, Madness]}

hungerOfThousands :: CardDef
hungerOfThousands =
  (treachery "z-circus-ex-mortis-213" "Hunger of Thousands" Set.ThousandToOne 2) {cdCardTraits = setFromList [Attack]}

ireOfShubNiggurath :: CardDef
ireOfShubNiggurath =
  (treachery "z-circus-ex-mortis-214" "Ire of Shub-Niggurath" Set.ThousandToOne 2) {cdCardTraits = setFromList [Terror, Curse]}

-- children_of_the_goat
endlessSpawn :: CardDef
endlessSpawn =
  (treachery "z-circus-ex-mortis-216" "Endless Spawn" Set.ChildrenOfTheGoat 2) {cdCardTraits = setFromList [Power]}

-- cult_of_shub-niggurath
milkOfShubNiggurath :: CardDef
milkOfShubNiggurath =
  (treachery "z-circus-ex-mortis-226" "Milk of Shub-Niggurath" Set.CultOfShubNiggurath 2) {cdCardTraits = setFromList [Power]}

-- illusory_tricks
hypnoticGlamour :: CardDef
hypnoticGlamour =
  (treachery "z-circus-ex-mortis-243" "Hypnotic Glamour" Set.IllusoryTricks 2) {cdCardTraits = setFromList [Hex]}

phantasmalDeception :: CardDef
phantasmalDeception =
  (treachery "z-circus-ex-mortis-244" "Phantasmal Deception" Set.IllusoryTricks 2) {cdCardTraits = setFromList [Scheme, Hex]}

-- lunatic_night
lunarInfluence :: CardDef
lunarInfluence =
  (treachery "z-circus-ex-mortis-245" "Lunar Influence" Set.LunaticNight 3) {cdCardTraits = setFromList [Mystery, Terror]}

ominousMoonlight :: CardDef
ominousMoonlight =
  (treachery "z-circus-ex-mortis-247" "Ominous Moonlight" Set.LunaticNight 3) {cdCardTraits = setFromList [Mystery, Omen]}

-- new_moon_daredevils
recklessStunt :: CardDef
recklessStunt =
  (treachery "z-circus-ex-mortis-251" "Reckless Stunt" Set.NewMoonDaredevils 2) {cdCardTraits = setFromList [Scheme, Hazard]}

-- new_moon_entertainers
quickerThanTheEye :: CardDef
quickerThanTheEye =
  (treachery "z-circus-ex-mortis-255" "Quicker Than the Eye" Set.NewMoonEntertainers 2) {cdCardTraits = setFromList [Scheme]}

-- panicked_masses
lostAllControl :: CardDef
lostAllControl =
  (treachery "z-circus-ex-mortis-257" "Lost All Control" Set.PanickedMasses 2) {cdCardTraits = setFromList [Madness]}

wildHysteria :: CardDef
wildHysteria =
  (treachery "z-circus-ex-mortis-258" "Wild Hysteria" Set.PanickedMasses 2) {cdCardTraits = setFromList [Terror, Blunder]}

-- primordial_evils
primordialEvils :: CardDef
primordialEvils =
  (treachery "z-circus-ex-mortis-259" "Primordial Evils" Set.PrimordialEvils 3) {cdCardTraits = setFromList [Omen]}

-- savage_woods
denseTangle :: CardDef
denseTangle =
  (treachery "z-circus-ex-mortis-260" "Dense Tangle" Set.SavageWoods 2) {cdCardTraits = setFromList [Obstacle]}

lostTheTrail :: CardDef
lostTheTrail =
  (treachery "z-circus-ex-mortis-261" "Lost the Trail" Set.SavageWoods 2) {cdCardTraits = setFromList [Blunder]}
