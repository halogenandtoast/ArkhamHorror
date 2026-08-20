{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.TheDrownedCity.TheWesternWall where

import Arkham.Location.CardDefs.Import

drownedShanty :: CardDef
drownedShanty =
  locationWithUnrevealed_
    "11527"
    "Treacherous Paths"
    [Rlyeh, Walkway]
    "Drowned Shanty"
    [Rlyeh, Walkway]
    TheWesternWall

obsidianFoundations :: CardDef
obsidianFoundations =
  locationWithUnrevealed_
    "11529"
    "Treacherous Paths"
    [Rlyeh, Walkway]
    "Obsidian Foundations"
    [Rlyeh]
    TheWesternWall

shatteredRuins :: CardDef
shatteredRuins =
  victory 1
    $ locationWithUnrevealed_
      "11528"
      "Treacherous Paths"
      [Rlyeh, Walkway]
      "Shattered Ruins"
      [Rlyeh, Walkway, Glyph]
      TheWesternWall

sunkenStairway :: CardDef
sunkenStairway =
  locationWithUnrevealed_
    "11526"
    "Treacherous Paths"
    [Rlyeh, Walkway]
    "Sunken Stairway"
    [Rlyeh, Walkway]
    TheWesternWall

treacherousPathDeadlyPass :: CardDef
treacherousPathDeadlyPass =
  locationWithUnrevealed_
    "11524"
    "Treacherous Paths"
    [Rlyeh, Walkway]
    ("Treacherous Path" <:> "Deadly Pass")
    [Rlyeh, Walkway]
    TheWesternWall

treacherousPathErodedShelf :: CardDef
treacherousPathErodedShelf =
  locationWithUnrevealed_
    "11522"
    "Treacherous Paths"
    [Rlyeh, Walkway]
    ("Treacherous Path" <:> "Eroded Shelf")
    [Rlyeh, Walkway]
    TheWesternWall

treacherousPathPrecariousClimb :: CardDef
treacherousPathPrecariousClimb =
  locationWithUnrevealed_
    "11523"
    "Treacherous Paths"
    [Rlyeh, Walkway]
    ("Treacherous Path" <:> "Precarious Climb")
    [Rlyeh, Walkway]
    TheWesternWall

treacherousPathShallowDen :: CardDef
treacherousPathShallowDen =
  locationWithUnrevealed_
    "11525"
    "Treacherous Paths"
    [Rlyeh, Walkway]
    ("Treacherous Path" <:> "Shallow Den")
    [Rlyeh, Walkway]
    TheWesternWall

treacherousPathSlickSteps :: CardDef
treacherousPathSlickSteps =
  locationWithUnrevealed_
    "11521"
    "Treacherous Paths"
    [Rlyeh, Walkway]
    ("Treacherous Path" <:> "Slick Steps")
    [Rlyeh, Walkway]
    TheWesternWall

underseaVault :: CardDef
underseaVault =
  storyOnBack' "11532b"
    $ victory 1
    $ location_ "11532" "Undersea Vault" [Vault, Glyph] TheWesternWall

westernWall_11530 :: CardDef
westernWall_11530 =
  location_ "11530" "Western Wall" [Rlyeh] TheWesternWall
