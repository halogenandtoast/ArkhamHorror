module Arkham.Location.CardDefs.TheScarletKeys.DogsOfWar where

import Arkham.Location.CardDefs.Import

catacombsOfKomElShoqafaAncientTomb :: CardDef
catacombsOfKomElShoqafaAncientTomb =
  victory 1
    $ location
      "09652"
      ("Catacombs of Kom el Shoqafa" <:> "Ancient Tomb")
      [Alexandria]
      Squiggle
      [Equals, Moon]
      DogsOfWar

catacombsOfKomElShoqafaBloodyNexus :: CardDef
catacombsOfKomElShoqafaBloodyNexus =
  victory 1
    $ location
      "09651"
      ("Catacombs of Kom el Shoqafa" <:> "Bloody Nexus")
      [Alexandria]
      Squiggle
      [Equals, Moon]
      DogsOfWar

catacombsOfKomElShoqafaDenOfTheBeast :: CardDef
catacombsOfKomElShoqafaDenOfTheBeast =
  victory 1
    $ location
      "09650"
      ("Catacombs of Kom el Shoqafa" <:> "Den of the Beast")
      [Alexandria]
      Squiggle
      [Equals, Moon]
      DogsOfWar

qaitbayCitadel :: CardDef
qaitbayCitadel =
  location
    "09647"
    "Qaitbay Citadel"
    [Alexandria]
    Star
    [Circle, Triangle, Equals, Moon]
    DogsOfWar

theBourseCommercialCenter :: CardDef
theBourseCommercialCenter =
  location
    "09644"
    ("The Bourse" <:> "Commercial Center")
    [Alexandria]
    Square
    [Circle, Triangle]
    DogsOfWar

theBourseCoteriePost :: CardDef
theBourseCoteriePost =
  victory 1
    $ location
      "09643"
      ("The Bourse" <:> "Coterie Post")
      [Alexandria]
      Square
      [Circle, Triangle]
      DogsOfWar

theBourseLocusSafeguard :: CardDef
theBourseLocusSafeguard =
  location
    "09642"
    ("The Bourse" <:> "Locus Safeguard")
    [Alexandria]
    Square
    [Circle, Triangle]
    DogsOfWar

theCorniche :: CardDef
theCorniche =
  location
    "09648"
    "The Corniche"
    [Alexandria, LocusSite]
    Equals
    [Squiggle, Moon, Star]
    DogsOfWar

victoriaCollege :: CardDef
victoriaCollege =
  location
    "09646"
    "Victoria College"
    [Alexandria, LocusSite]
    Triangle
    [Square, Circle, Star]
    DogsOfWar

windsorPalaceHotel :: CardDef
windsorPalaceHotel =
  location
    "09645"
    "Windsor Palace Hotel"
    [Alexandria, LocusSite]
    Circle
    [Square, Triangle, Star]
    DogsOfWar

zanEtElSettat :: CardDef
zanEtElSettat =
  location
    "09649"
    "Zan'et el Settat"
    [Alexandria, LocusSite]
    Moon
    [Squiggle, Equals, Star]
    DogsOfWar
