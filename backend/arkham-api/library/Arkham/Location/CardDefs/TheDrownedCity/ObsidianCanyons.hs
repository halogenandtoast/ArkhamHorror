{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.TheDrownedCity.ObsidianCanyons where

import Arkham.Location.CardDefs.Import

aerialWaterfall :: CardDef
aerialWaterfall =
  locationWithUnrevealed_ "11660" summit [Summit] "Aerial Waterfall" [Summit] ObsidianCanyons

ancientCanyons :: CardDef
ancientCanyons =
  locationWithUnrevealed_ "11658" summit [Summit] "Ancient Canyons" [Summit] ObsidianCanyons

ancientDome :: CardDef
ancientDome =
  locationWithUnrevealed_ "11652" summit [Summit] "Ancient Dome" [Rlyeh, Central] ObsidianCanyons

centralSpire :: CardDef
centralSpire =
  locationWithUnrevealed_ "11649" summit [Summit] "Central Spire" [Rlyeh, Central] ObsidianCanyons

dazzlingSkyline :: CardDef
dazzlingSkyline =
  quantity 3
    $ locationWithUnrevealed_ "11659" summit [Summit] "Dazzling Skyline" [Summit] ObsidianCanyons

easternAthenaeum :: CardDef
easternAthenaeum =
  victory 1
    $ locationWithUnrevealed_
      "11653"
      summit
      [Summit]
      "Eastern Athenaeum"
      [Rlyeh, Summit, Glyph]
      ObsidianCanyons

floatingSpire :: CardDef
floatingSpire =
  locationWithUnrevealed_ "11650" summit [Summit] "Floating Spire" [Rlyeh, Central] ObsidianCanyons

glyphOrrery :: CardDef
glyphOrrery =
  storyOnBack' "11662b"
    $ victory 1
    $ location_ "11662" "Glyph Orrery" [Rlyeh, Summit, Glyph] ObsidianCanyons

hangingShip :: CardDef
hangingShip =
  quantity 2 $ locationWithUnrevealed_ "11657" summit [Summit] "Hanging Ship" [Summit] ObsidianCanyons

magneticSpires :: CardDef
magneticSpires =
  quantity 2
    $ locationWithUnrevealed_ "11661" summit [Summit] "Magnetic Spires" [Summit] ObsidianCanyons

obsidianCliffs :: CardDef
obsidianCliffs =
  victory 1
    $ locationWithUnrevealed_ "11655" summit [Summit] "Obsidian Cliffs" [Rlyeh, Summit] ObsidianCanyons

rlyehStreets :: CardDef
rlyehStreets =
  location_ "11648" "R'lyeh Streets" [Rlyeh, Central] ObsidianCanyons

suspendedReef :: CardDef
suspendedReef =
  quantity 3
    $ locationWithUnrevealed_ "11656" summit [Summit] "Suspended Reef" [Rlyeh, Summit] ObsidianCanyons

westernAthenaeum :: CardDef
westernAthenaeum =
  victory 1
    $ locationWithUnrevealed_
      "11654"
      summit
      [Summit]
      "Western Athenaeum"
      [Rlyeh, Summit, Glyph]
      ObsidianCanyons

westernWall_11651 :: CardDef
westernWall_11651 =
  locationWithUnrevealed_ "11651" summit [Summit] "Western Wall" [Rlyeh, Central] ObsidianCanyons
