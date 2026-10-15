{- | The house of Usher.

Every location but The Tarn is double-sided: the @a@ face is [[Decadent]] and
the @b@ face is the [[Ruined]] room it collapses into once it has taken damage
equal to its shroud. Both faces print the same symbol and connections, so the
map never rewires.
-}
module Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations where

import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Sets qualified as Set
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Traits
import Arkham.Location.CardDefs.Import

decadent :: CardCode -> Name -> LocationSymbol -> [LocationSymbol] -> CardDef
decadent cardCode name symbol connections =
  otherSideIs (flippedCardCode cardCode)
    $ location cardCode name [HouseOfUsher, Decadent] symbol connections Set.TheFallOfTheHouseOfUsher

ruined :: CardCode -> Name -> LocationSymbol -> [LocationSymbol] -> CardDef
ruined cardCode name symbol connections =
  otherSideIs cardCode
    $ location
      (flippedCardCode cardCode)
      name
      [HouseOfUsher, Ruined]
      symbol
      connections
      Set.TheFallOfTheHouseOfUsher

grandHall :: CardDef
grandHall = decadent ":the-fall-of-the-house-of-usher:007" "Grand Hall" Diamond grandHallConnections

grandHallRuined :: CardDef
grandHallRuined = ruined ":the-fall-of-the-house-of-usher:007" "Grand Hall" Diamond grandHallConnections

grandHallConnections :: [LocationSymbol]
grandHallConnections = [Star, Equals, Squiggle, Circle, Plus, Hourglass]

rodericksStudio :: CardDef
rodericksStudio = decadent ":the-fall-of-the-house-of-usher:008" "Roderick's Studio" Star [Equals, Diamond]

rodericksStudioRuined :: CardDef
rodericksStudioRuined = ruined ":the-fall-of-the-house-of-usher:008" "Roderick's Studio" Star [Equals, Diamond]

privateLibrary :: CardDef
privateLibrary =
  victory 1 $ decadent ":the-fall-of-the-house-of-usher:009" "Private Library" Equals [Star, Diamond]

privateLibraryRuined :: CardDef
privateLibraryRuined =
  victory 1 $ ruined ":the-fall-of-the-house-of-usher:009" "Private Library" Equals [Star, Diamond]

portraitGallery :: CardDef
portraitGallery = decadent ":the-fall-of-the-house-of-usher:010" "Portrait Gallery" Squiggle [Diamond, Plus]

portraitGalleryRuined :: CardDef
portraitGalleryRuined = ruined ":the-fall-of-the-house-of-usher:010" "Portrait Gallery" Squiggle [Diamond, Plus]

banquetHall :: CardDef
banquetHall = decadent ":the-fall-of-the-house-of-usher:011" "Banquet Hall" Circle [Diamond, Hourglass]

banquetHallRuined :: CardDef
banquetHallRuined = ruined ":the-fall-of-the-house-of-usher:011" "Banquet Hall" Circle [Diamond, Hourglass]

manorChapel :: CardDef
manorChapel = decadent ":the-fall-of-the-house-of-usher:012" "Manor Chapel" Plus [Diamond, Squiggle, Heart]

manorChapelRuined :: CardDef
manorChapelRuined = ruined ":the-fall-of-the-house-of-usher:012" "Manor Chapel" Plus [Diamond, Squiggle, Heart]

wineCellar :: CardDef
wineCellar = decadent ":the-fall-of-the-house-of-usher:013" "Wine Cellar" Hourglass wineCellarConnections

wineCellarRuined :: CardDef
wineCellarRuined = ruined ":the-fall-of-the-house-of-usher:013" "Wine Cellar" Hourglass wineCellarConnections

wineCellarConnections :: [LocationSymbol]
wineCellarConnections = [Diamond, Circle, Heart, T]

usherFamilyCrypt :: CardDef
usherFamilyCrypt =
  victory 1
    $ decadent ":the-fall-of-the-house-of-usher:014" "Usher Family Crypt" Heart [Plus, Hourglass, T]

usherFamilyCryptRuined :: CardDef
usherFamilyCryptRuined =
  victory 1
    $ ruined ":the-fall-of-the-house-of-usher:014" "Usher Family Crypt" Heart [Plus, Hourglass, T]

theTarn :: CardDef
theTarn =
  victory 1
    $ location
      ":the-fall-of-the-house-of-usher:015"
      "The Tarn"
      [HouseOfUsher, Cursed]
      T
      [Hourglass, Heart]
      Set.TheFallOfTheHouseOfUsher
