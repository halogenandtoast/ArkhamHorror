module Arkham.Location.CardDefs.TheInnsmouthConspiracy.FloodedCaverns where

import Arkham.Location.CardDefs.Import

tidalPool :: CardDef
tidalPool =
  quantity 2
    $ locationWithUnrevealed
      "07103"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      "Tidal Pool"
      [Cave]
      NoSymbol
      []
      FloodedCaverns

undergroundRiver :: CardDef
undergroundRiver =
  victory 1
    $ quantity 2
    $ locationWithUnrevealed
      "07104"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      "Underground River"
      [Cave]
      NoSymbol
      []
      FloodedCaverns

underwaterCavern :: CardDef
underwaterCavern =
  quantity 2
    $ locationWithUnrevealed
      "07102"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      "Underwater Cavern"
      [Cave]
      NoSymbol
      []
      FloodedCaverns
