{- | The SS Constellation.

Cargo Room and Open Water start in play; the other ten are set aside and put
into play together when act 1 advances -- except Lifeboat, which act 3a ("Flee
the Ship") puts out on its own.

The @Deck@ traits order the ship vertically. Water rises from the bottom: a card
that floods "a ready location with the lowest possible @Deck@ number" always
takes a @Deck 1@ room first. Open Water and Lifeboat carry no @Deck@ trait, so
they never flood.
-}
module Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Locations where

import Arkham.Homebrew.ConsternationOnTheConstellation.Sets qualified as Set
import Arkham.Homebrew.ConsternationOnTheConstellation.Traits
import Arkham.Location.CardDefs.Import

-- * Deck 1

engineRoom :: CardDef
engineRoom =
  ( location
      ":consternation-on-the-constellation:015"
      "Engine Room"
      [Deck1]
      Circle
      [Square, Plus, Triangle]
      Set.ConsternationOnTheConstellation
  )
    { cdVictoryPoints = Just 1
    }

boilerRoom :: CardDef
boilerRoom =
  ( location
      ":consternation-on-the-constellation:016"
      "Boiler Room"
      [Deck1]
      Square
      [Circle]
      Set.ConsternationOnTheConstellation
  )
    { cdVictoryPoints = Just 1
    }

-- | In play from setup; every investigator starts here.
cargoRoom :: CardDef
cargoRoom =
  location
    ":consternation-on-the-constellation:017"
    "Cargo Room"
    [Deck1]
    Triangle
    [Circle, Heart, Star]
    Set.ConsternationOnTheConstellation

-- * Deck 2

galley :: CardDef
galley =
  location
    ":consternation-on-the-constellation:018"
    "Galley"
    [Deck2]
    Plus
    [Circle, Diamond]
    Set.ConsternationOnTheConstellation

diningRoom :: CardDef
diningRoom =
  location
    ":consternation-on-the-constellation:019"
    "Dining Room"
    [Deck2]
    Diamond
    [Plus, Squiggle, Hourglass]
    Set.ConsternationOnTheConstellation

passengerCabins :: CardDef
passengerCabins =
  location
    ":consternation-on-the-constellation:020"
    "Passenger Cabins"
    [Deck2]
    Squiggle
    [Diamond, T]
    Set.ConsternationOnTheConstellation

library :: CardDef
library =
  ( location
      ":consternation-on-the-constellation:021"
      "Library"
      [Deck2]
      T
      [Squiggle, Moon]
      Set.ConsternationOnTheConstellation
  )
    { cdVictoryPoints = Just 5
    }

-- * Deck 3

deckLoungeAndTheatre :: CardDef
deckLoungeAndTheatre =
  location
    ":consternation-on-the-constellation:022"
    "Deck Lounge and Theatre"
    [Deck3]
    Hourglass
    [Equals, Diamond]
    Set.ConsternationOnTheConstellation

sunDeck :: CardDef
sunDeck =
  location
    ":consternation-on-the-constellation:023"
    "Sun Deck"
    [Deck3]
    Moon
    [T, Equals]
    Set.ConsternationOnTheConstellation

bridge :: CardDef
bridge =
  ( location
      ":consternation-on-the-constellation:024"
      "Bridge"
      [Deck3]
      Equals
      [Hourglass, Moon]
      Set.ConsternationOnTheConstellation
  )
    { cdVictoryPoints = Just 1
    }

-- * Off the ship

-- | In play from setup. Carries no @Deck@ trait, so it never floods.
openWater :: CardDef
openWater =
  location
    ":consternation-on-the-constellation:025"
    "Open Water"
    [Ocean]
    Star
    [Triangle, Plus]
    Set.ConsternationOnTheConstellation

-- | Set aside until act 3a ("Flee the Ship") lifts it into position.
lifeboat :: CardDef
lifeboat =
  ( location
      ":consternation-on-the-constellation:026"
      "Lifeboat"
      [Boat]
      Heart
      [Triangle]
      Set.ConsternationOnTheConstellation
  )
    { cdVictoryPoints = Just 2
    }
