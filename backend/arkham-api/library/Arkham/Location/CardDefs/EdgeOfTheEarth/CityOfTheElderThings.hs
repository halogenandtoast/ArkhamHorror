module Arkham.Location.CardDefs.EdgeOfTheEarth.CityOfTheElderThings where

import Arkham.Location.CardDefs.Import

hiddenTunnelEntranceToTheDepths :: CardDef
hiddenTunnelEntranceToTheDepths =
  victory 1
    $ location
      "08630"
      ("Hidden Tunnel" <:> "Entrance to the Depths")
      [City]
      NoSymbol
      []
      CityOfTheElderThings
