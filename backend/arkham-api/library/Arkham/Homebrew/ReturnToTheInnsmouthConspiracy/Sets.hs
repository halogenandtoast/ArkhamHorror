{- | The fourteen encounter sets of The (Unofficial) Return to The Innsmouth
Conspiracy. Eight modify one scenario each; five replace an official set
outright; Return to Flooded Caverns is combined with the original set rather
than replacing it (see the campaign's rules insert).
-}
module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets (
  module Arkham.EncounterSet,
  pattern BarricadedDoors,
  pattern InnsmouthHaze,
  pattern Occultation,
  pattern ReturnToALightInTheFog,
  pattern ReturnToDevilReef,
  pattern ReturnToFloodedCaverns,
  pattern ReturnToHorrorInHighGear,
  pattern ReturnToInTooDeep,
  pattern ReturnToIntoTheMaelstrom,
  pattern ReturnToTheLairOfDagon,
  pattern ReturnToThePitOfDespair,
  pattern ReturnToTheVanishingOfElinaHarper,
  pattern RollingTide,
  pattern StalkersOfCthulhu,
) where

import Arkham.EncounterSet

pattern ReturnToThePitOfDespair :: EncounterSet
pattern ReturnToThePitOfDespair =
  Homebrew ":return-to-the-innsmouth-conspiracy:return_to_the_pit_of_despair"

pattern ReturnToTheVanishingOfElinaHarper :: EncounterSet
pattern ReturnToTheVanishingOfElinaHarper =
  Homebrew ":return-to-the-innsmouth-conspiracy:return_to_the_vanishing_of_elina_harper"

pattern ReturnToInTooDeep :: EncounterSet
pattern ReturnToInTooDeep = Homebrew ":return-to-the-innsmouth-conspiracy:return_to_in_too_deep"

pattern ReturnToDevilReef :: EncounterSet
pattern ReturnToDevilReef = Homebrew ":return-to-the-innsmouth-conspiracy:return_to_devil_reef"

pattern ReturnToHorrorInHighGear :: EncounterSet
pattern ReturnToHorrorInHighGear =
  Homebrew ":return-to-the-innsmouth-conspiracy:return_to_horror_in_high_gear"

pattern ReturnToALightInTheFog :: EncounterSet
pattern ReturnToALightInTheFog = Homebrew ":return-to-the-innsmouth-conspiracy:return_to_a_light_in_the_fog"

pattern ReturnToTheLairOfDagon :: EncounterSet
pattern ReturnToTheLairOfDagon = Homebrew ":return-to-the-innsmouth-conspiracy:return_to_the_lair_of_dagon"

pattern ReturnToIntoTheMaelstrom :: EncounterSet
pattern ReturnToIntoTheMaelstrom =
  Homebrew ":return-to-the-innsmouth-conspiracy:return_to_into_the_maelstrom"

pattern StalkersOfCthulhu :: EncounterSet
pattern StalkersOfCthulhu = Homebrew ":return-to-the-innsmouth-conspiracy:stalkers_of_cthulhu"

pattern RollingTide :: EncounterSet
pattern RollingTide = Homebrew ":return-to-the-innsmouth-conspiracy:rolling_tide"

pattern InnsmouthHaze :: EncounterSet
pattern InnsmouthHaze = Homebrew ":return-to-the-innsmouth-conspiracy:innsmouth_haze"

pattern Occultation :: EncounterSet
pattern Occultation = Homebrew ":return-to-the-innsmouth-conspiracy:occultation"

pattern BarricadedDoors :: EncounterSet
pattern BarricadedDoors = Homebrew ":return-to-the-innsmouth-conspiracy:barricaded_doors"

pattern ReturnToFloodedCaverns :: EncounterSet
pattern ReturnToFloodedCaverns = Homebrew ":return-to-the-innsmouth-conspiracy:return_to_flooded_caverns"
