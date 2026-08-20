module Arkham.Location.CardDefs.NightOfTheZealot.TheDevourerBelow where

import Arkham.Location.CardDefs.Import

arkhamWoodsCliffside :: CardDef
arkhamWoodsCliffside =
  locationWithUnrevealed
    "01153"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Cliffside")
    [Woods]
    Hourglass
    [Squiggle, Moon, Triangle]
    TheDevourerBelow

arkhamWoodsOldHouse :: CardDef
arkhamWoodsOldHouse =
  locationWithUnrevealed
    "01152"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Old House")
    [Woods]
    Diamond
    [Squiggle, Triangle, T]
    TheDevourerBelow

arkhamWoodsQuietGlade :: CardDef
arkhamWoodsQuietGlade =
  locationWithUnrevealed
    "01155"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Quiet Glade")
    [Woods]
    Moon
    [Squiggle, Equals, Hourglass]
    TheDevourerBelow

arkhamWoodsTangledThicket :: CardDef
arkhamWoodsTangledThicket =
  locationWithUnrevealed
    "01154"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Tangled Thicket")
    [Woods]
    Equals
    [Squiggle, T, Moon]
    TheDevourerBelow

arkhamWoodsTwistingPaths :: CardDef
arkhamWoodsTwistingPaths =
  locationWithUnrevealed
    "01151"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Twisting Paths")
    [Woods]
    T
    [Squiggle, Diamond, Equals]
    TheDevourerBelow

arkhamWoodsUnhallowedGround :: CardDef
arkhamWoodsUnhallowedGround =
  locationWithUnrevealed
    "01150"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Unhallowed Ground")
    [Woods]
    Triangle
    [Squiggle, Hourglass, Diamond]
    TheDevourerBelow

mainPath :: CardDef
mainPath =
  location "01149" "Main Path" [Woods] Squiggle [Square, Plus] TheDevourerBelow

ritualSite :: CardDef
ritualSite =
  location "01156" "Ritual Site" [Cave] Plus [Squiggle] TheDevourerBelow
