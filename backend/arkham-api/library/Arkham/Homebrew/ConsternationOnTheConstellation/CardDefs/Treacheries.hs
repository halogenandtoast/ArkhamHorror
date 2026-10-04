{- | Consternation on the Constellation's treacheries.

Three encounter sets' worth. The Consternation set's seven are the encounter
deck proper; the Deep Ones and Sinking Ship sets are set aside at setup and
shuffled in when act 2 (or agenda 2) advances and the creatures board.
-}
module Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Treacheries where

import Arkham.Homebrew.ConsternationOnTheConstellation.Sets qualified as Set
import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

-- * Consternation on the Constellation

lightsOut :: CardDef
lightsOut =
  ( treachery
      ":consternation-on-the-constellation:031"
      "Lights Out"
      Set.ConsternationOnTheConstellation
      2
  )
    { cdCardTraits = singleton Hazard
    }

sealedDoors :: CardDef
sealedDoors =
  ( treachery
      ":consternation-on-the-constellation:032"
      "Sealed Doors"
      Set.ConsternationOnTheConstellation
      2
  )
    { cdCardTraits = singleton Obstacle
    }

seasickness :: CardDef
seasickness =
  ( treachery
      ":consternation-on-the-constellation:033"
      "Seasickness"
      Set.ConsternationOnTheConstellation
      2
  )
    { cdCardTraits = singleton Hazard
    }

oceansMaw :: CardDef
oceansMaw =
  ( treachery
      ":consternation-on-the-constellation:034"
      "Ocean's Maw"
      Set.ConsternationOnTheConstellation
      2
  )
    { cdCardTraits = singleton Hex
    }

thalassophobia :: CardDef
thalassophobia =
  ( treachery
      ":consternation-on-the-constellation:035"
      "Thalassophobia"
      Set.ConsternationOnTheConstellation
      2
  )
    { cdCardTraits = singleton Terror
    }

riteOfTheDeep :: CardDef
riteOfTheDeep =
  ( treachery
      ":consternation-on-the-constellation:036"
      "Rite of the Deep"
      Set.ConsternationOnTheConstellation
      2
  )
    { cdCardTraits = singleton Hex
    }

clapOfThunder :: CardDef
clapOfThunder =
  ( treachery
      ":consternation-on-the-constellation:037"
      "Clap of Thunder"
      Set.ConsternationOnTheConstellation
      2
  )
    { cdCardTraits = singleton Omen
    }

-- * Deep Ones

callOfRlyeh :: CardDef
callOfRlyeh =
  (treachery ":consternation-on-the-constellation:042" "Call of R'lyeh" Set.DeepOnes 2)
    { cdCardTraits = singleton Omen
    }

-- * Sinking Ship

{- | Setup removes one copy per player beyond the first, so a four-handed game
plays with two rather than five.
-}
takingOnWater :: CardDef
takingOnWater =
  (treachery ":consternation-on-the-constellation:043" "Taking on Water" Set.SinkingShip 5)
    { cdCardTraits = singleton Hazard
    , cdKeywords = singleton Keyword.Peril
    }

outOfAir :: CardDef
outOfAir =
  (treachery ":consternation-on-the-constellation:044" "Out of Air" Set.SinkingShip 2)
    { cdCardTraits = singleton Hazard
    }

sweptAway :: CardDef
sweptAway =
  (treachery ":consternation-on-the-constellation:045" "Swept Away" Set.SinkingShip 2)
    { cdCardTraits = singleton Hazard
    }
