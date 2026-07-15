module Arkham.Homebrew.DarkMatter.CardDefs.Agendas where

import Arkham.Agenda.CardDefs.Import
import Arkham.Homebrew.DarkMatter.Sets qualified as Set

-- the_tatterdemalion
emergencyProcedure :: CardDef
emergencyProcedure =
  agenda ":dark-matter:014" "Emergency Procedure" 1 Set.TheTatterdemalion

theGhostShip :: CardDef
theGhostShip = agenda ":dark-matter:015" "The Ghost Ship" 2 Set.TheTatterdemalion

riseOfTheMachines :: CardDef
riseOfTheMachines =
  agenda ":dark-matter:016" "Rise of the Machines" 3 Set.TheTatterdemalion

-- electric_nightmare
figmentOfYourImagination :: CardDef
figmentOfYourImagination =
  agenda ":dark-matter:054" "Figment of Your Imagination" 1 Set.ElectricNightmare

it :: CardDef
it = agenda ":dark-matter:055" "IT" 2 Set.ElectricNightmare

-- lost_quantum
theQuantumMaelstrom_091 :: CardDef
theQuantumMaelstrom_091 =
  otherSideIs ":dark-matter:090ab" $ agenda ":dark-matter:090aa" "The Quantum Maelstrom" 1 Set.LostQuantum

theQuantumMaelstrom_092 :: CardDef
theQuantumMaelstrom_092 =
  otherSideIs ":dark-matter:090bb" $ agenda ":dark-matter:090ba" "The Quantum Maelstrom" 1 Set.LostQuantum

theQuantumMaelstrom_093 :: CardDef
theQuantumMaelstrom_093 =
  otherSideIs ":dark-matter:090cb" $ agenda ":dark-matter:090ca" "The Quantum Maelstrom" 1 Set.LostQuantum

-- in_the_shadow_of_earth
theNostalgiaII :: CardDef
theNostalgiaII =
  agenda ":dark-matter:113" "The Nostalgia II" 1 Set.InTheShadowOfEarth

theThingFromEarth :: CardDef
theThingFromEarth =
  agenda ":dark-matter:114" "The Thing from Earth" 2 Set.InTheShadowOfEarth

screamOfTheDead :: CardDef
screamOfTheDead =
  agenda ":dark-matter:115" "Scream of the Dead" 3 Set.InTheShadowOfEarth

itsWeirdAndPissedOff :: CardDef
itsWeirdAndPissedOff =
  agenda ":dark-matter:119" "It's Weird and Pissed Off." 4 Set.InTheShadowOfEarth

-- strange_moons
moonsOfSaturn :: CardDef
moonsOfSaturn = agenda ":dark-matter:154" "Moons of Saturn" 1 Set.StrangeMoons

signsFromAldebaran :: CardDef
signsFromAldebaran =
  agenda ":dark-matter:155" "Signs from Aldebaran" 2 Set.StrangeMoons

flightOfTheByakhees :: CardDef
flightOfTheByakhees =
  agenda ":dark-matter:156" "Flight of the Byakhees" 3 Set.StrangeMoons

againstTheSun :: CardDef
againstTheSun = agenda ":dark-matter:157" "Against the Sun" 4 Set.StrangeMoons

-- the_machine_in_yellow
theThirdAct :: CardDef
theThirdAct = agenda ":dark-matter:191" "The Third Act" 1 Set.TheMachineInYellow

aNightmare :: CardDef
aNightmare = agenda ":dark-matter:192" "A Nightmare" 2 Set.TheMachineInYellow

outOfMind :: CardDef
outOfMind = agenda ":dark-matter:193" "Out of Mind" 3 Set.TheMachineInYellow

-- fragment_of_carcosa
theShadowsLengthen :: CardDef
theShadowsLengthen =
  agenda ":dark-matter:210" "The Shadows Lengthen" 1 Set.FragmentOfCarcosa

shallDryAndDie :: CardDef
shallDryAndDie =
  agenda ":dark-matter:211" "Shall Dry and Die" 2 Set.FragmentOfCarcosa

-- starfall
journeyAcrossSpace :: CardDef
journeyAcrossSpace =
  agenda ":dark-matter:244" "Journey Across Space" 1 Set.Starfall

redSun :: CardDef
redSun = agenda ":dark-matter:245" "Red Sun" 2 Set.Starfall

supernova :: CardDef
supernova = agenda ":dark-matter:246" "Supernova" 3 Set.Starfall

darkMatter :: CardDef
darkMatter = agenda ":dark-matter:247" "Dark Matter" 1 Set.Starfall
