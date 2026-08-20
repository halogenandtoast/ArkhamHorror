module Arkham.Location.CardDefs.ReturnToThePathToCarcosa.ReturnToCurtainCall where

import Arkham.Location.CardDefs.Import

propShop :: CardDef
propShop =
  victory 1
    $ locationWithUnrevealed
      "52019"
      "Backstage Doorway"
      [Private]
      Moon
      [Diamond]
      "Prop Shop"
      [Private]
      Moon
      [Diamond]
      ReturnToCurtainCall

theatreLounge :: CardDef
theatreLounge =
  victory 1
    $ locationWithUnrevealed
      "52018"
      "Lobby Doorway"
      [Private]
      Plus
      [Triangle]
      "Theatre Lounge"
      [Private]
      Plus
      [Triangle]
      ReturnToCurtainCall
