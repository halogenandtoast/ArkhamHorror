module Arkham.Location.CardDefs.ThePathToCarcosa.CurtainCall where

import Arkham.Location.CardDefs.Import

backstage :: CardDef
backstage =
  location "03052" "Backstage" mempty Diamond [Circle, Moon] CurtainCall

balcony :: CardDef
balcony =
  victory 1
    $ location "03051" "Balcony" mempty Square [Circle, Triangle] CurtainCall

boxOffice :: CardDef
boxOffice =
  locationWithUnrevealed
    "03054"
    "Lobby Doorway"
    [Private]
    Plus
    [Triangle]
    "Box Office"
    [Private]
    Plus
    [Triangle]
    CurtainCall

dressingRoom :: CardDef
dressingRoom =
  locationWithUnrevealed
    "03056"
    "Backstage Doorway"
    [Private]
    Moon
    [Diamond]
    "Dressing Room"
    [Private]
    Moon
    [Diamond]
    CurtainCall

greenRoom :: CardDef
greenRoom =
  victory 1
    $ locationWithUnrevealed
      "03055"
      "Lobby Doorway"
      [Private]
      Plus
      [Triangle]
      "Green Room"
      [Private]
      Plus
      [Triangle]
      CurtainCall

lightingBox :: CardDef
lightingBox =
  victory 1
    $ locationWithUnrevealed
      "03053"
      "Lobby Doorway"
      [Private]
      Plus
      [Triangle]
      "Lighting Box"
      [Private]
      Plus
      [Triangle]
      CurtainCall

lobby :: CardDef
lobby =
  location "03050" "Lobby" mempty Triangle [Circle, Square, Plus] CurtainCall

rehearsalRoom :: CardDef
rehearsalRoom =
  victory 1
    $ locationWithUnrevealed
      "03057"
      "Backstage Doorway"
      [Private]
      Moon
      [Diamond]
      "Rehearsal Room"
      [Private]
      Moon
      [Diamond]
      CurtainCall

theatre :: CardDef
theatre =
  location "03049" "Theatre" mempty Circle [Diamond, Triangle] CurtainCall

trapRoom :: CardDef
trapRoom =
  victory 1
    $ locationWithUnrevealed
      "03058"
      "Backstage Doorway"
      [Private]
      Moon
      [Diamond]
      "Trap Room"
      [Private]
      Moon
      [Diamond]
      CurtainCall
