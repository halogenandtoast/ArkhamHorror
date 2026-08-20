module Arkham.Location.CardDefs.TheCircleUndone.TheWitchingHour where

import Arkham.Location.CardDefs.Import
import Arkham.Trait qualified as Trait

witchHauntedWoodsAbandonedMine :: CardDef
witchHauntedWoodsAbandonedMine =
  victory 1
    $ locationWithUnrevealed
      "05058"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Abandoned Mine")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsCairnStones :: CardDef
witchHauntedWoodsCairnStones =
  victory 1
    $ locationWithUnrevealed
      "05059"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Cairn Stones")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsChildsTreeHouse :: CardDef
witchHauntedWoodsChildsTreeHouse =
  victory 1
    $ locationWithUnrevealed
      "05061"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Child's Tree House")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsHermitsHouse :: CardDef
witchHauntedWoodsHermitsHouse =
  victory 1
    $ locationWithUnrevealed
      "05063"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Hermit's House")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsOvergrownBarn :: CardDef
witchHauntedWoodsOvergrownBarn =
  victory 1
    $ locationWithUnrevealed
      "05064"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Overgrown Barn")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsTaintedWell :: CardDef
witchHauntedWoodsTaintedWell =
  victory 1
    $ locationWithUnrevealed
      "05062"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Tainted Well")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsTheLonelyTree :: CardDef
witchHauntedWoodsTheLonelyTree =
  victory 1
    $ locationWithUnrevealed
      "05060"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "The Lonely Tree")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchesCircle :: CardDef
witchesCircle =
  revelation
    $ victory 2
    $ otherSideIs "05055"
    $ location "05055b" "Witches' Circle" [Woods, Trait.Circle] Plus [Squiggle] TheWitchingHour
