{- | Arkham's Northside, after dark.

Five of the eight places come in two printings with the same name and the same
connections; setup puts one of each pair on the board at random and sets the
other aside, and act 1 lays those five out in the arrangement. The pairs are
told apart here by their collector number, the way the base game tells two
Quiet Halls apart.

The red connection symbol the designer draws as a saltire is the game's
'Plus' -- the engine has one red cross symbol, and this is it.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Locations where

import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Sets qualified as Set
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Traits
import Arkham.Location.CardDefs.Import

northside :: CardCode -> Name -> LocationSymbol -> [LocationSymbol] -> CardDef
northside cardCode name symbol connections =
  location cardCode name [Northside] symbol connections Set.TheStrangeCaseOfDrJekyllAndMrHyde

northsidePlaza :: CardDef
northsidePlaza =
  victory 1
    $ northside
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:007"
      "Northside Plaza"
      Square
      [Circle, Plus, Triangle, Diamond, Heart]

drugstore_008 :: CardDef
drugstore_008 =
  northside
    ":the-strange-case-of-dr-jekyll-and-mr-hyde:008"
    "Drugstore"
    Plus
    [Square, Equals, Triangle, Diamond, Circle]

drugstore_009 :: CardDef
drugstore_009 =
  northside
    ":the-strange-case-of-dr-jekyll-and-mr-hyde:009"
    "Drugstore"
    Plus
    [Square, Equals, Triangle, Diamond, Circle]

importsOffice_010 :: CardDef
importsOffice_010 =
  northside
    ":the-strange-case-of-dr-jekyll-and-mr-hyde:010"
    "Imports Office"
    Triangle
    [Square, Plus, Diamond]

importsOffice_011 :: CardDef
importsOffice_011 =
  northside
    ":the-strange-case-of-dr-jekyll-and-mr-hyde:011"
    "Imports Office"
    Triangle
    [Square, Plus, Diamond]

etchersShop_012 :: CardDef
etchersShop_012 =
  northside
    ":the-strange-case-of-dr-jekyll-and-mr-hyde:012"
    "Etchers' Shop"
    Diamond
    [Square, Plus, Triangle, Hourglass, Heart]

etchersShop_013 :: CardDef
etchersShop_013 =
  northside
    ":the-strange-case-of-dr-jekyll-and-mr-hyde:013"
    "Etchers' Shop"
    Diamond
    [Square, Plus, Triangle, Hourglass, Heart]

chemicalFirm_014 :: CardDef
chemicalFirm_014 =
  northside
    ":the-strange-case-of-dr-jekyll-and-mr-hyde:014"
    "Chemical Firm"
    Heart
    [Square, Hourglass, Diamond]

chemicalFirm_015 :: CardDef
chemicalFirm_015 =
  northside
    ":the-strange-case-of-dr-jekyll-and-mr-hyde:015"
    "Chemical Firm"
    Heart
    [Square, Hourglass, Diamond]

tannery_016 :: CardDef
tannery_016 =
  northside
    ":the-strange-case-of-dr-jekyll-and-mr-hyde:016"
    "Tannery"
    Circle
    [Square, Equals, Plus]

tannery_017 :: CardDef
tannery_017 =
  northside
    ":the-strange-case-of-dr-jekyll-and-mr-hyde:017"
    "Tannery"
    Circle
    [Square, Equals, Plus]

trainyard :: CardDef
trainyard =
  victory 1
    $ northside ":the-strange-case-of-dr-jekyll-and-mr-hyde:018" "Trainyard" Equals [Circle, Plus]

abandonedFactory :: CardDef
abandonedFactory =
  victory 1
    $ northside
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:019"
      "Abandoned Factory"
      Hourglass
      [Heart, Diamond]
