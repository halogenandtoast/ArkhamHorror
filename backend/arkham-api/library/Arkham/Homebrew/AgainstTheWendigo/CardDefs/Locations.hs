{- | Against the Wendigo's map.

The valley is a four-by-three grid. The bottom row (Sarcee Territory, Jetty,
Fort McDonald) and the middle column (three North Hanninah) are fixed; the six
Uncharted locations are shuffled into the two outer columns at setup, so every
one of them prints the same connection rule -- "the River location directly to
the East or West" -- and the scenario wires the actual connections from the
grid.

A location's printed symbol is cosmetic here for that reason: the cards connect
by compass direction, not by symbol.
-}
module Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations where

import Arkham.Homebrew.AgainstTheWendigo.Sets qualified as Set
import Arkham.Homebrew.AgainstTheWendigo.Traits
import Arkham.Location.CardDefs.Import

-- The bottom row: where the investigators start and where they can resign.

jetty :: CardDef
jetty =
  location
    ":against-the-wendigo:010"
    "Jetty"
    [Civilized, River]
    Squiggle
    [Plus, Triangle, Diamond]
    Set.HanninahValley

fortMcDonald :: CardDef
fortMcDonald =
  location
    ":against-the-wendigo:012"
    "Fort McDonald"
    [Civilized]
    Diamond
    [Squiggle]
    Set.HanninahValley

sarceeTerritory :: CardDef
sarceeTerritory =
  locationWithUnrevealed
    ":against-the-wendigo:020"
    "Sarcee Territory"
    [Civilized, Sarcee]
    Plus
    [Squiggle]
    ("Imala Foxtail" <:> "Alone at Home")
    [Civilized, Sarcee]
    Plus
    [Squiggle]
    Set.HanninahValley

-- The middle column: the river north out of the Jetty.

northHanninah1 :: CardDef
northHanninah1 =
  location
    ":against-the-wendigo:013"
    "North Hanninah"
    [Wild, River]
    Triangle
    []
    Set.HanninahValley

northHanninah2 :: CardDef
northHanninah2 =
  location
    ":against-the-wendigo:014"
    "North Hanninah"
    [Wild, River]
    Triangle
    []
    Set.HanninahValley

northHanninah3 :: CardDef
northHanninah3 =
  location
    ":against-the-wendigo:015"
    "North Hanninah"
    [Wild, River]
    Triangle
    []
    Set.HanninahValley

{- | The six Uncharted locations. Seven are printed; setup removes one of the two
Mountain Ranges at random, shuffles the rest, and deals one to the East and one
to the West of each North Hanninah.
-}
templeOfIthaqua :: CardDef
templeOfIthaqua =
  victory 0
    $ locationWithUnrevealed
      ":against-the-wendigo:008"
      "Mountain Range"
      [Wild]
      Square
      [Triangle]
      "Temple of Ithaqua"
      [Wild, Mystical]
      Square
      [Triangle]
      Set.HanninahValley

madProspector :: CardDef
madProspector =
  victory 1
    $ locationWithUnrevealed
      ":against-the-wendigo:009"
      "Mountain Range"
      [Wild]
      Square
      [Triangle]
      "Mad Prospector"
      [Wild]
      Square
      [Triangle]
      Set.HanninahValley

impenetrableForest :: CardDef
impenetrableForest =
  victory 1
    $ locationWithUnrevealed
      ":against-the-wendigo:011"
      "Impenetrable Forest"
      [Wild]
      Equals
      [Triangle]
      "Impenetrable Forest"
      [Wild]
      Equals
      [Triangle, Heart]
      Set.HanninahValley

swamp :: CardDef
swamp =
  victory 1
    $ locationWithUnrevealed
      ":against-the-wendigo:016"
      "Swamp"
      [Wild]
      Moon
      [Triangle]
      "Swamp"
      [Wild]
      Moon
      [Triangle]
      Set.HanninahValley

siteOfAncientStones :: CardDef
siteOfAncientStones =
  victory 1
    $ locationWithUnrevealed
      ":against-the-wendigo:017"
      "Site of Ancient Stones"
      [Wild, Mystical]
      Hourglass
      [Triangle]
      "Site of Ancient Stones"
      [Wild, Mystical]
      Hourglass
      [Triangle]
      Set.HanninahValley

sinisterTaiga :: CardDef
sinisterTaiga =
  location
    ":against-the-wendigo:018"
    "Sinister Taiga"
    [Wild]
    T
    [Triangle]
    Set.HanninahValley

hiddenHut :: CardDef
hiddenHut =
  locationWithUnrevealed
    ":against-the-wendigo:019"
    "Isolated Land"
    [Wild]
    Circle
    [Triangle]
    "Hidden Hut"
    [Wild, Sarcee]
    Circle
    [Triangle]
    Set.HanninahValley

-- Locations that only ever arrive from the back of a story card.

{- | The back of The Knowledge of the Cold. It replaces the Temple of Ithaqua on
the board, so it never enters the Uncharted pool.
-}
ithaqua :: CardDef
ithaqua =
  ( location
      ":against-the-wendigo:023b"
      "Ithaqua"
      [Mystical]
      Star
      [Triangle]
      Set.HanninahValley
  )
    { cdOtherSide = Just ":against-the-wendigo:023"
    }

-- | The back of Sylvia's Fate (v. I). Put into play beside the Impenetrable Forest.
theHeartOfTheForest :: CardDef
theHeartOfTheForest =
  ( location
      ":against-the-wendigo:028b"
      "The Heart of the Forest"
      [Wild, Mystical]
      Heart
      [Equals]
      Set.HanninahValley
  )
    { cdOtherSide = Just ":against-the-wendigo:028"
    }
