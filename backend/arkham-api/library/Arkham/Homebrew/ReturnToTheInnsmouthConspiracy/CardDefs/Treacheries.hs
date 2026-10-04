module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries where

import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Set
import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

-- return_to_the_pit_of_despair

lostInTheCaves :: CardDef
lostInTheCaves =
  ( treachery
      ":return-to-the-innsmouth-conspiracy:020"
      "Lost in the Caves"
      Set.ReturnToThePitOfDespair
      2
  )
    { cdCardTraits = singleton Blunder
    , cdKeywords = singleton Keyword.Surge
    }

troublingMemories :: CardDef
troublingMemories =
  ( treachery
      ":return-to-the-innsmouth-conspiracy:021"
      "Troubling Memories"
      Set.ReturnToThePitOfDespair
      2
  )
    { cdCardTraits = singleton Terror
    }

-- return_to_the_vanishing_of_elina_harper

{- | Shares its name with the official Growing Suspicion agenda (07058) but is a
different card: this one is a treachery.
-}
growingSuspicion :: CardDef
growingSuspicion =
  ( treachery
      ":return-to-the-innsmouth-conspiracy:024"
      "Growing Suspicion"
      Set.ReturnToTheVanishingOfElinaHarper
      2
  )
    { cdCardTraits = singleton Scheme
    }

-- return_to_in_too_deep

{- | Printed on a player back: it is set aside and added to a Deep One investigator's
deck when the act advances, and being permanent it starts every later scenario in play,
which is what keeps them a Deep One investigator for the rest of the campaign.
-}
innsmouthInfluence :: CardDef
innsmouthInfluence =
  (weakness ":return-to-the-innsmouth-conspiracy:030" "Innsmouth Influence")
    { cdCardTraits = singleton Curse
    , cdPermanent = True
    , cdEncounterSet = Just Set.ReturnToInTooDeep
    , cdEncounterSetQuantity = Just 4
    }

-- return_to_horror_in_high_gear

wrecked :: CardDef
wrecked =
  (treachery ":return-to-the-innsmouth-conspiracy:038" "Wrecked!" Set.ReturnToHorrorInHighGear 2)
    { cdCardTraits = singleton Blunder
    }

-- return_to_a_light_in_the_fog

bornToBreed :: CardDef
bornToBreed =
  (treachery ":return-to-the-innsmouth-conspiracy:041" "Born to Breed" Set.ReturnToALightInTheFog 2)
    { cdCardTraits = singleton Omen
    }

-- return_to_the_lair_of_dagon

stirringInHisSleep :: CardDef
stirringInHisSleep =
  ( treachery
      ":return-to-the-innsmouth-conspiracy:047"
      "Stirring in His Sleep"
      Set.ReturnToTheLairOfDagon
      1
  )
    { cdCardTraits = setFromList [Hazard, Omen]
    , cdVictoryPoints = Just 1
    }

-- return_to_into_the_maelstrom

presenceOfTheFather :: CardDef
presenceOfTheFather =
  ( treachery
      ":return-to-the-innsmouth-conspiracy:052"
      "Presence of the Father"
      Set.ReturnToIntoTheMaelstrom
      1
  )
    { cdCardTraits = singleton Power
    , cdKeywords = singleton Keyword.Surge
    }

presenceOfTheMother :: CardDef
presenceOfTheMother =
  ( treachery
      ":return-to-the-innsmouth-conspiracy:053"
      "Presence of the Mother"
      Set.ReturnToIntoTheMaelstrom
      1
  )
    { cdCardTraits = singleton Power
    , cdKeywords = singleton Keyword.Surge
    }

stirringInTheirSleep :: CardDef
stirringInTheirSleep =
  ( treachery
      ":return-to-the-innsmouth-conspiracy:054"
      "Stirring in Their Sleep"
      Set.ReturnToIntoTheMaelstrom
      2
  )
    { cdCardTraits = setFromList [Hazard, Omen]
    , cdVictoryPoints = Just 1
    }

-- barricaded_doors (replaces Locked Doors, Core Set)

barricadedDoor :: CardDef
barricadedDoor =
  (treachery ":return-to-the-innsmouth-conspiracy:055" "Barricaded Door" Set.BarricadedDoors 2)
    { cdCardTraits = singleton Obstacle
    }

-- innsmouth_haze (replaces Fog over Innsmouth)

innsmouthHaze :: CardDef
innsmouthHaze =
  (treachery ":return-to-the-innsmouth-conspiracy:057" "Innsmouth Haze" Set.InnsmouthHaze 2)
    { cdCardTraits = singleton Hazard
    }

-- occultation (replaces Syzygy)

kingTide :: CardDef
kingTide =
  (treachery ":return-to-the-innsmouth-conspiracy:058" "King Tide" Set.Occultation 2)
    { cdCardTraits = setFromList [Hazard, Omen]
    , cdKeywords = singleton Keyword.Peril
    }

occultation :: CardDef
occultation =
  (treachery ":return-to-the-innsmouth-conspiracy:059" "Occultation" Set.Occultation 2)
    { cdCardTraits = singleton Omen
    }

-- rolling_tide (replaces Rising Tide)

callOfTheSea :: CardDef
callOfTheSea =
  (treachery ":return-to-the-innsmouth-conspiracy:060" "Call of the Sea" Set.RollingTide 2)
    { cdCardTraits = singleton Power
    }

rollingTide :: CardDef
rollingTide =
  (treachery ":return-to-the-innsmouth-conspiracy:061" "Rolling Tide" Set.RollingTide 2)
    { cdCardTraits = singleton Hazard
    }

struggleForAir :: CardDef
struggleForAir =
  (treachery ":return-to-the-innsmouth-conspiracy:062" "Struggle for Air" Set.RollingTide 2)
    { cdCardTraits = singleton Hazard
    }

-- stalkers_of_cthulhu (replaces Agents of Cthulhu)

stalkedByDeepOnes :: CardDef
stalkedByDeepOnes =
  (treachery ":return-to-the-innsmouth-conspiracy:064" "Stalked by Deep Ones" Set.StalkersOfCthulhu 2)
    { cdCardTraits = singleton Scheme
    }
