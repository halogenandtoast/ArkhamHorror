module Arkham.Location.CardDefs.ReturnToTheCircleUndone.ReturnToTheWitchingHour where

import Arkham.Location.CardDefs.Import

arkhamWoodsBootleggingOperation :: CardDef
arkhamWoodsBootleggingOperation =
  locationWithUnrevealed
    "54023"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Bootlegging Operation")
    [Woods]
    Trefoil
    [Squiggle, Equals, Hourglass]
    ReturnToTheWitchingHour

arkhamWoodsHiddenPath :: CardDef
arkhamWoodsHiddenPath =
  locationWithUnrevealed
    "54021"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Hidden Path")
    [Woods]
    Spade
    [Squiggle, Trefoil]
    ReturnToTheWitchingHour

arkhamWoodsPlaceOfPower :: CardDef
arkhamWoodsPlaceOfPower =
  locationWithUnrevealed
    "54022"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Place of Power")
    [Woods]
    Trefoil
    [Squiggle, Spade]
    ReturnToTheWitchingHour

witchHauntedWoodsUnmarkedGraveyard :: CardDef
witchHauntedWoodsUnmarkedGraveyard =
  victory 1
    $ locationWithUnrevealed
      "54020"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Unmarked Graveyard")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ReturnToTheWitchingHour

witchHauntedWoodsWitchTree :: CardDef
witchHauntedWoodsWitchTree =
  victory 1
    $ locationWithUnrevealed
      "54019"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Witch Tree")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ReturnToTheWitchingHour
