module Arkham.Location.CardDefs.TheForgottenAge.TheUntamedWilds where

import Arkham.Location.CardDefs.Import

expeditionCamp :: CardDef
expeditionCamp =
  location "04050" "Expedition Camp" [Campsite, Jungle] Circle [Square, Diamond, Moon] TheUntamedWilds

ruinsOfEztli :: CardDef
ruinsOfEztli =
  victory 2
    $ singleSided
    $ location "04053" "Ruins of Eztli" [Ancient, Ruins] Hourglass [Triangle, Heart] TheUntamedWilds
