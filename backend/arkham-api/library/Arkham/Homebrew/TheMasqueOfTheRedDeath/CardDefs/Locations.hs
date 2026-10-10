{- | Prospero's French Hill manor.

The seven coloured chambers are a *chain*, not a grid: Grand Ballroom
(Hourglass) - Blue (Triangle) - Purple (Star) - Green (Diamond) - Orange (Heart)
- White (Circle) - Violet (Moon) - Black (T). Black Chamber is the terminal room
and connects only to the Violet Chamber, which is why act 1 asks the whole party
to reach it.

Every location is in play from setup and every one of them has text on both
faces: the unrevealed face prints the additional cost to enter the chamber, and
the revealed face prints the chamber's own @[skull]@ effect. So the usual
reveal-on-entry is what turns a toll gate into a hazard, and act 1's advance
flips Grand Ballroom back to its unrevealed side so act 2 can charge for it
again.
-}
module Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Locations where

import Arkham.Homebrew.TheMasqueOfTheRedDeath.Sets qualified as Set
import Arkham.Location.CardDefs.Import

grandBallroom :: CardDef
grandBallroom =
  location
    ":the-masque-of-the-red-death:007"
    "Grand Ballroom"
    [Manor, Central]
    Hourglass
    [Triangle]
    Set.TheMasqueOfTheRedDeath

blueChamber :: CardDef
blueChamber =
  location
    ":the-masque-of-the-red-death:008"
    "Blue Chamber"
    [Manor]
    Triangle
    [Hourglass, Star]
    Set.TheMasqueOfTheRedDeath

purpleChamber :: CardDef
purpleChamber =
  location
    ":the-masque-of-the-red-death:009"
    "Purple Chamber"
    [Manor]
    Star
    [Triangle, Diamond]
    Set.TheMasqueOfTheRedDeath

greenChamber :: CardDef
greenChamber =
  location
    ":the-masque-of-the-red-death:010"
    "Green Chamber"
    [Manor]
    Diamond
    [Star, Heart]
    Set.TheMasqueOfTheRedDeath

orangeChamber :: CardDef
orangeChamber =
  victory 1
    $ location
      ":the-masque-of-the-red-death:011"
      "Orange Chamber"
      [Manor]
      Heart
      [Diamond, Circle]
      Set.TheMasqueOfTheRedDeath

whiteChamber :: CardDef
whiteChamber =
  location
    ":the-masque-of-the-red-death:012"
    "White Chamber"
    [Manor]
    Circle
    [Heart, Moon]
    Set.TheMasqueOfTheRedDeath

violetChamber :: CardDef
violetChamber =
  location
    ":the-masque-of-the-red-death:013"
    "Violet Chamber"
    [Manor]
    Moon
    [Circle, T]
    Set.TheMasqueOfTheRedDeath

-- | Prospero's sanctum. The terminal link in the chain and act 1's objective.
blackChamber :: CardDef
blackChamber =
  victory 1
    $ location
      ":the-masque-of-the-red-death:014"
      "Black Chamber"
      [Manor]
      T
      [Moon]
      Set.TheMasqueOfTheRedDeath
