module Arkham.Location.CardDefs.ReturnToTheForgottenAge.ReturnToTheCityOfArchives where

import Arkham.Location.CardDefs.Import

alienConservatory :: CardDef
alienConservatory =
  location
    "53057"
    "Alien Conservatory"
    [Ancient, Pnakotus]
    Heart
    [Plus, Hourglass]
    ReturnToTheCityOfArchives

cyclopeanVaults :: CardDef
cyclopeanVaults =
  victory 1
    $ location
      "53056"
      "Cyclopean Vaults"
      [Ancient, Pnakotus]
      Hourglass
      [Plus, Heart]
      ReturnToTheCityOfArchives

hallsOfPnakotusSouthernCorridors :: CardDef
hallsOfPnakotusSouthernCorridors =
  location
    "53055"
    ("Halls of Pnakotus" <:> "Southern Corridors")
    [Ancient, Pnakotus]
    Plus
    [Square, Diamond, Heart, Hourglass]
    ReturnToTheCityOfArchives
