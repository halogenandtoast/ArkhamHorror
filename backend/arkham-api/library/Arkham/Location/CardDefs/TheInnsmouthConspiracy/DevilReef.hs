{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.TheInnsmouthConspiracy.DevilReef where

import Arkham.Location.CardDefs.Import

blackReef :: CardDef
blackReef =
  locationWithUnrevealed
    "07173"
    "Devil Reef"
    [Ocean, Island]
    Circle
    [Triangle]
    "Black Reef"
    [Ocean, Island]
    Plus
    [Triangle, Square, Star]
    DevilReef

bootleggersHideaway_174a :: CardDef
bootleggersHideaway_174a =
  victory 1
    $ locationWithUnrevealed_
      "07174a"
      "Tidal Tunnel"
      [Cave]
      "Bootlegger's Hideaway"
      [Cave]
      DevilReef

bootleggersHideaway_174b :: CardDef
bootleggersHideaway_174b =
  victory 1
    $ locationWithUnrevealed_
      "07174b"
      "Tidal Tunnel"
      [Cave]
      "Bootlegger's Hideaway"
      [Cave]
      DevilReef

churningWaters :: CardDef
churningWaters =
  location
    "07168"
    "Churning Waters"
    [Ocean]
    Triangle
    [Circle, Square, Heart, Star, Diamond, Plus]
    DevilReef

cyclopeanRuins_176a :: CardDef
cyclopeanRuins_176a =
  locationWithUnrevealed_
    "07176a"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    "Cyclopean Ruins"
    [Cave, Yhanthlei]
    DevilReef

cyclopeanRuins_176b :: CardDef
cyclopeanRuins_176b =
  locationWithUnrevealed_
    "07176b"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    "Cyclopean Ruins"
    [Cave, Yhanthlei]
    DevilReef

deepOneGrotto_175a :: CardDef
deepOneGrotto_175a =
  locationWithUnrevealed_
    "07175a"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    "Deep One Grotto"
    [Cave, Yhanthlei]
    DevilReef

deepOneGrotto_175b :: CardDef
deepOneGrotto_175b =
  locationWithUnrevealed_
    "07175b"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    "Deep One Grotto"
    [Cave, Yhanthlei]
    DevilReef

hiddenCove :: CardDef
hiddenCove =
  locationWithUnrevealed
    "07170"
    "Devil Reef"
    [Ocean, Island]
    Circle
    [Triangle]
    "Hidden Cove"
    [Ocean, Island]
    Heart
    [Triangle, Diamond, Star]
    DevilReef

lonelyIsle :: CardDef
lonelyIsle =
  locationWithUnrevealed
    "07169"
    "Devil Reef"
    [Ocean, Island]
    Circle
    [Triangle]
    "Lonely Isle"
    [Ocean, Island]
    Square
    [Triangle, Plus, Diamond]
    DevilReef

saltMarshes :: CardDef
saltMarshes =
  locationWithUnrevealed
    "07172"
    "Devil Reef"
    [Ocean, Island]
    Circle
    [Triangle]
    "Salt Marshes"
    [Ocean, Island]
    Diamond
    [Triangle, Heart, Square]
    DevilReef

templeOfTheUnion_177a :: CardDef
templeOfTheUnion_177a =
  locationWithUnrevealed_
    "07177a"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    "Temple of the Union"
    [Cave, Yhanthlei]
    DevilReef

templeOfTheUnion_177b :: CardDef
templeOfTheUnion_177b =
  locationWithUnrevealed_
    "07177b"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    "Temple of the Union"
    [Cave, Yhanthlei]
    DevilReef

wavewornIsland :: CardDef
wavewornIsland =
  locationWithUnrevealed
    "07171"
    "Devil Reef"
    [Ocean, Island]
    Circle
    [Triangle]
    "Waveworn Island"
    [Ocean, Island]
    Star
    [Triangle, Plus, Heart]
    DevilReef
