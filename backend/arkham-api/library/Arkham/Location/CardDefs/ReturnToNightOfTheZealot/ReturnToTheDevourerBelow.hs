module Arkham.Location.CardDefs.ReturnToNightOfTheZealot.ReturnToTheDevourerBelow where

import Arkham.Location.CardDefs.Import

arkhamWoodsCorpseRiddenClearing :: CardDef
arkhamWoodsCorpseRiddenClearing =
  locationWithUnrevealed
    "50035"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Corpse-Ridden Clearing")
    [Woods]
    Droplet
    [Squiggle, Circle]
    ReturnToTheDevourerBelow

arkhamWoodsGreatWillow :: CardDef
arkhamWoodsGreatWillow =
  locationWithUnrevealed
    "50033"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Great Willow")
    [Woods]
    Heart
    [Squiggle, Star]
    ReturnToTheDevourerBelow

arkhamWoodsLakeside :: CardDef
arkhamWoodsLakeside =
  locationWithUnrevealed
    "50034"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Lakeside")
    [Woods]
    Star
    [Squiggle, Heart]
    ReturnToTheDevourerBelow

arkhamWoodsWoodenBridge :: CardDef
arkhamWoodsWoodenBridge =
  locationWithUnrevealed
    "50036"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Wooden Bridge")
    [Woods]
    Circle
    [Squiggle, Droplet]
    ReturnToTheDevourerBelow
