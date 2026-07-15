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
    $ (story ":dark-matter:030" "Final Destination" Set.TheTatterdemalion) {cdVictoryPoints = Just 1}

intoTheArchives :: CardDef
intoTheArchives =
  withScanIcons [LS.Trefoil]
    $ (story ":dark-matter:032" "Into the Archives" Set.TheTatterdemalion) {cdVictoryPoints = Just 1}

k2PS187CyberneticBrains :: CardDef
k2PS187CyberneticBrains =
  withScanIcons [LS.T, LS.Trefoil]
    $ (story ":dark-matter:034" "K2-PS187 Cybernetic Brains" Set.TheTatterdemalion) {cdVictoryPoints = Just 1}

strangeIsTheNight :: CardDef
strangeIsTheNight =
  withScanIcons [LS.Star]
    $ (story ":dark-matter:039" "Strange is the Night" Set.TheTatterdemalion) {cdVictoryPoints = Just 1}

whatTypeOfShipIsThis :: CardDef
whatTypeOfShipIsThis =
  withScanIcons [LS.Moon, LS.Hourglass]
    $ (story ":dark-matter:041" "What Type of Ship is This?" Set.TheTatterdemalion) {cdVictoryPoints = Just 2}

whoAmI :: CardDef
whoAmI =
  withScanIcons [LS.Triangle]
    $ (story ":dark-matter:042" "Who am I?" Set.TheTatterdemalion) {cdVictoryPoints = Just 1}

-- Dark Matter (fan campaign by Axolotl): electric_nightmare
reintegrated_062 :: CardDef
reintegrated_062 =
  otherSideIs ":dark-matter:063ab" $ story ":dark-matter:063aa" "Reintegrated" Set.ElectricNightmare

reintegrated_063 :: CardDef
reintegrated_063 =
  otherSideIs ":dark-matter:063bb" $ story ":dark-matter:063ba" "Reintegrated" Set.ElectricNightmare

reintegrated_064 :: CardDef
reintegrated_064 =
  otherSideIs ":dark-matter:063cb" $ story ":dark-matter:063ca" "Reintegrated" Set.ElectricNightmare

reintegrated_065 :: CardDef
reintegrated_065 =
  otherSideIs ":dark-matter:063db" $ story ":dark-matter:063da" "Reintegrated" Set.ElectricNightmare

-- Dark Matter (fan campaign by Axolotl): in_the_shadow_of_earth
evidenceAdamTanner :: CardDef
evidenceAdamTanner =
  story ":dark-matter:136" "Evidence (Adam Tanner)" Set.InTheShadowOfEarth

evidenceCaptainBurr :: CardDef
evidenceCaptainBurr =
  story ":dark-matter:137" "Evidence (Captain Burr)" Set.InTheShadowOfEarth

evidenceDoctorFeng :: CardDef
evidenceDoctorFeng =
  story ":dark-matter:138" "Evidence (Doctor Feng)" Set.InTheShadowOfEarth

evidenceLtArcherMichaels :: CardDef
evidenceLtArcherMichaels =
  story ":dark-matter:139" "Evidence (Lt. \"Archer\" Michaels)" Set.InTheShadowOfEarth

evidenceMUD12Mudbug :: CardDef
evidenceMUD12Mudbug =
  story ":dark-matter:140" "Evidence (MU-D12 \"Mudbug\")" Set.InTheShadowOfEarth

evidenceSophie :: CardDef
evidenceSophie =
  story ":dark-matter:141" "Evidence (Sophie)" Set.InTheShadowOfEarth

-- Dark Matter (fan campaign by Axolotl): strange_moons
theCultist :: CardDef
theCultist = story ":dark-matter:176" "The Cultist" Set.StrangeMoons

theMiner :: CardDef
theMiner = story ":dark-matter:177" "The Miner" Set.StrangeMoons

theTeacher :: CardDef
theTeacher = story ":dark-matter:178" "The Teacher" Set.StrangeMoons

-- Dark Matter (fan campaign by Axolotl): fragment_of_carcosa
arrivalOfTheKing :: CardDef
arrivalOfTheKing =
  story ":dark-matter:224" "Arrival of the King" Set.FragmentOfCarcosa

delights :: CardDef
delights = story ":dark-matter:225" "Delights" Set.FragmentOfCarcosa

forYouAlone :: CardDef
forYouAlone = story ":dark-matter:226" "For You Alone" Set.FragmentOfCarcosa

lostExpedition :: CardDef
lostExpedition = story ":dark-matter:227" "Lost Expedition" Set.FragmentOfCarcosa

-- Dark Matter (fan campaign by Axolotl): starfall
ritualOfTheSun :: CardDef
ritualOfTheSun = story ":dark-matter:266" "Ritual of the Sun" Set.Starfall

withoutATrace :: CardDef
withoutATrace = story ":dark-matter:275" "Without a Trace" Set.Starfall

--- Circus Ex Mortis (homebrew)
