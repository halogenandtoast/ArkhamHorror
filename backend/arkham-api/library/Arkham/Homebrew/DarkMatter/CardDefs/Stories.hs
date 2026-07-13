module Arkham.Homebrew.DarkMatter.CardDefs.Stories where

import Arkham.Card.CardDef
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.LocationSymbol qualified as LS
import Arkham.Prelude
import Arkham.Story.CardDefs.Base

-- Dark Matter (fan campaign by Axolotl): the_tatterdemalion
finalDestination :: CardDef
finalDestination =
  withScanIcons [LS.T, LS.Trefoil]
    $ (story "z-dark-matter-030" "Final Destination" Set.TheTatterdemalion) {cdVictoryPoints = Just 1}

intoTheArchives :: CardDef
intoTheArchives =
  withScanIcons [LS.Trefoil]
    $ (story "z-dark-matter-032" "Into the Archives" Set.TheTatterdemalion) {cdVictoryPoints = Just 1}

k2PS187CyberneticBrains :: CardDef
k2PS187CyberneticBrains =
  withScanIcons [LS.T, LS.Trefoil]
    $ (story "z-dark-matter-034" "K2-PS187 Cybernetic Brains" Set.TheTatterdemalion) {cdVictoryPoints = Just 1}

strangeIsTheNight :: CardDef
strangeIsTheNight =
  withScanIcons [LS.Star]
    $ (story "z-dark-matter-039" "Strange is the Night" Set.TheTatterdemalion) {cdVictoryPoints = Just 1}

whatTypeOfShipIsThis :: CardDef
whatTypeOfShipIsThis =
  withScanIcons [LS.Moon, LS.Hourglass]
    $ (story "z-dark-matter-041" "What Type of Ship is This?" Set.TheTatterdemalion) {cdVictoryPoints = Just 2}

whoAmI :: CardDef
whoAmI =
  withScanIcons [LS.Triangle]
    $ (story "z-dark-matter-042" "Who am I?" Set.TheTatterdemalion) {cdVictoryPoints = Just 1}

-- Dark Matter (fan campaign by Axolotl): electric_nightmare
reintegrated_062 :: CardDef
reintegrated_062 =
  doubleSided $ story "z-dark-matter-062" "Reintegrated" Set.ElectricNightmare

reintegrated_063 :: CardDef
reintegrated_063 =
  doubleSided $ story "z-dark-matter-063" "Reintegrated" Set.ElectricNightmare

reintegrated_064 :: CardDef
reintegrated_064 =
  doubleSided $ story "z-dark-matter-064" "Reintegrated" Set.ElectricNightmare

reintegrated_065 :: CardDef
reintegrated_065 =
  doubleSided $ story "z-dark-matter-065" "Reintegrated" Set.ElectricNightmare

-- Dark Matter (fan campaign by Axolotl): in_the_shadow_of_earth
evidenceAdamTanner :: CardDef
evidenceAdamTanner =
  story "z-dark-matter-139" "Evidence (Adam Tanner)" Set.InTheShadowOfEarth

evidenceCaptainBurr :: CardDef
evidenceCaptainBurr =
  story "z-dark-matter-140" "Evidence (Captain Burr)" Set.InTheShadowOfEarth

evidenceDoctorFeng :: CardDef
evidenceDoctorFeng =
  story "z-dark-matter-141" "Evidence (Doctor Feng)" Set.InTheShadowOfEarth

evidenceLtArcherMichaels :: CardDef
evidenceLtArcherMichaels =
  story "z-dark-matter-142" "Evidence (Lt. \"Archer\" Michaels)" Set.InTheShadowOfEarth

evidenceMUD12Mudbug :: CardDef
evidenceMUD12Mudbug =
  story "z-dark-matter-143" "Evidence (MU-D12 \"Mudbug\")" Set.InTheShadowOfEarth

evidenceSophie :: CardDef
evidenceSophie =
  story "z-dark-matter-144" "Evidence (Sophie)" Set.InTheShadowOfEarth

-- Dark Matter (fan campaign by Axolotl): strange_moons
theCultist :: CardDef
theCultist = story "z-dark-matter-179" "The Cultist" Set.StrangeMoons

theMiner :: CardDef
theMiner = story "z-dark-matter-180" "The Miner" Set.StrangeMoons

theTeacher :: CardDef
theTeacher = story "z-dark-matter-181" "The Teacher" Set.StrangeMoons

-- Dark Matter (fan campaign by Axolotl): fragment_of_carcosa
arrivalOfTheKing :: CardDef
arrivalOfTheKing =
  story "z-dark-matter-227" "Arrival of the King" Set.FragmentOfCarcosa

delights :: CardDef
delights = story "z-dark-matter-228" "Delights" Set.FragmentOfCarcosa

forYouAlone :: CardDef
forYouAlone = story "z-dark-matter-229" "For You Alone" Set.FragmentOfCarcosa

lostExpedition :: CardDef
lostExpedition = story "z-dark-matter-230" "Lost Expedition" Set.FragmentOfCarcosa

-- Dark Matter (fan campaign by Axolotl): starfall
ritualOfTheSun :: CardDef
ritualOfTheSun = story "z-dark-matter-269" "Ritual of the Sun" Set.Starfall

withoutATrace :: CardDef
withoutATrace = story "z-dark-matter-278" "Without a Trace" Set.Starfall

--- Circus Ex Mortis (homebrew)
