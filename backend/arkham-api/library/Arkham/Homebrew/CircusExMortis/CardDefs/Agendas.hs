module Arkham.Homebrew.CircusExMortis.CardDefs.Agendas where

import Arkham.Agenda.CardDefs.Import
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set

-- one_night_only
theTrueFace :: CardDef
theTrueFace =
  agenda "z-circus-ex-mortis-002" "The True Face" 1 Set.OneNightOnly

houseOfHorrors :: CardDef
houseOfHorrors =
  agenda "z-circus-ex-mortis-003" "House of Horrors" 2 Set.OneNightOnly

mesmericMagic :: CardDef
mesmericMagic =
  agenda "z-circus-ex-mortis-004" "Mesmeric Magic" 3 Set.OneNightOnly

-- the_primrose_path
savageNature :: CardDef
savageNature =
  agenda "z-circus-ex-mortis-018" "Savage Nature" 1 Set.ThePrimrosePath

bloodMoon :: CardDef
bloodMoon =
  agenda "z-circus-ex-mortis-019" "Blood Moon" 2 Set.ThePrimrosePath

-- harm_s_way
theCircusSleeps :: CardDef
theCircusSleeps =
  agenda "z-circus-ex-mortis-043" "The Circus Sleeps" 1 Set.HarmsWay

treadingOnEggshells :: CardDef
treadingOnEggshells =
  agenda "z-circus-ex-mortis-044" "Treading on Eggshells" 2 Set.HarmsWay

sleepWhenYoureDead :: CardDef
sleepWhenYoureDead =
  agenda "z-circus-ex-mortis-045" "Sleep When You're Dead" 3 Set.HarmsWay

-- all_points_west
scheduleToKeep :: CardDef
scheduleToKeep =
  agenda "z-circus-ex-mortis-077" "Schedule to Keep" 1 Set.AllPointsWest

-- piper_at_the_gates_of_dawn
repeatShowing :: CardDef
repeatShowing =
  agenda "z-circus-ex-mortis-111" "Repeat Showing" 1 Set.PiperAtTheGatesOfDawn

doomAndGloom :: CardDef
doomAndGloom =
  agenda "z-circus-ex-mortis-112" "Doom and Gloom" 2 Set.PiperAtTheGatesOfDawn

whirlingSpectacle :: CardDef
whirlingSpectacle =
  agenda "z-circus-ex-mortis-113" "Whirling Spectacle" 3 Set.PiperAtTheGatesOfDawn

-- bacchanalia
intoTheLionsDen :: CardDef
intoTheLionsDen =
  agenda "z-circus-ex-mortis-125" "Into the Lion's Den" 1 Set.Bacchanalia

lackOfRestraint :: CardDef
lackOfRestraint =
  agenda "z-circus-ex-mortis-126" "Lack of Restraint" 2 Set.Bacchanalia

feverPitch :: CardDef
feverPitch = agenda "z-circus-ex-mortis-127" "Fever Pitch" 3 Set.Bacchanalia

-- red_sunrise
fadingSunlightVI :: CardDef
fadingSunlightVI =
  agenda "z-circus-ex-mortis-156" "Fading Sunlight (v.I)" 1 Set.RedSunrise

fadingSunlightVII :: CardDef
fadingSunlightVII =
  agenda "z-circus-ex-mortis-157" "Fading Sunlight (v.II)" 1 Set.RedSunrise

-- thousand_to_one
underMoonlessSkies :: CardDef
underMoonlessSkies =
  agenda "z-circus-ex-mortis-193" "Under Moonless Skies" 1 Set.ThousandToOne
