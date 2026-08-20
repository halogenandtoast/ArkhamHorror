module Arkham.Location.CardDefs.TheInnsmouthConspiracy.TheLairOfDagon where

import Arkham.Location.CardDefs.Import

doorwayToTheDepths :: CardDef
doorwayToTheDepths =
  locationWithUnrevealed
    "07290"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Doorway to the Depths"
    [Cave]
    Circle
    [Diamond]
    TheLairOfDagon

foulCorridors :: CardDef
foulCorridors =
  locationWithUnrevealed
    "07286"
    "Foul Corridors"
    [SecondFloor]
    Star
    [Triangle, Heart, Moon, Equals, Hourglass]
    "Foul Corridors"
    [SecondFloor, Passageway]
    Star
    [Triangle, Heart, Moon, Equals, Hourglass]
    TheLairOfDagon

grandEntryway :: CardDef
grandEntryway =
  location
    "07283"
    "Grand Entryway"
    [GroundFloor]
    Triangle
    [Square, Star, Plus, Trefoil]
    TheLairOfDagon

hallOfBlood :: CardDef
hallOfBlood =
  locationWithUnrevealed
    "07284"
    "First Floor Hall"
    [GroundFloor]
    Trefoil
    [Triangle]
    "Hall of Blood"
    [GroundFloor]
    Square
    [Triangle]
    TheLairOfDagon

hallOfLoyalty :: CardDef
hallOfLoyalty =
  victory 1
    $ locationWithUnrevealed
      "07287"
      "Second Floor Hall"
      [SecondFloor]
      Hourglass
      [Star]
      "Hall of Loyalty"
      [SecondFloor]
      Equals
      [Star]
      TheLairOfDagon

hallOfRebirth :: CardDef
hallOfRebirth =
  locationWithUnrevealed
    "07288"
    "Second Floor Hall"
    [SecondFloor]
    Hourglass
    [Star]
    "Hall of Rebirth"
    [SecondFloor]
    Heart
    [Star]
    TheLairOfDagon

hallOfSilence :: CardDef
hallOfSilence =
  locationWithUnrevealed
    "07289"
    "Third Floor Hall"
    [ThirdFloor]
    Moon
    [Star]
    "Hall of Silence"
    [ThirdFloor]
    Moon
    [Star]
    TheLairOfDagon

hallOfTheDeep :: CardDef
hallOfTheDeep =
  victory 1
    $ locationWithUnrevealed
      "07285"
      "First Floor Hall"
      [GroundFloor]
      Trefoil
      [Triangle]
      "Hall of the Deep"
      [GroundFloor]
      Plus
      [Triangle]
      TheLairOfDagon

lairOfDagon :: CardDef
lairOfDagon =
  victory 1
    $ location
      "07291"
      "Lair of Dagon"
      [Yhanthlei, Lair]
      Diamond
      [Circle]
      TheLairOfDagon
