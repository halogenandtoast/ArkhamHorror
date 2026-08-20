module Arkham.Location.CardDefs.TheScarletKeys.DancingMad where

import Arkham.Location.CardDefs.Import

cafeLunaBastionOfRemembrance :: CardDef
cafeLunaBastionOfRemembrance =
  otherSideIs "09600a"
    $ location
      "09600b"
      ("Café Luna" <:> "Bastion of Remembrance")
      [Havana]
      Spade
      [Squiggle, T]
      DancingMad

cafeLunaCoterieHaunt :: CardDef
cafeLunaCoterieHaunt =
  otherSideIs "09600b"
    $ location
      "09600a"
      ("Café Luna" <:> "Coterie Haunt")
      [Havana]
      Spade
      [Squiggle, T]
      DancingMad

elMalecon :: CardDef
elMalecon =
  victory 1
    $ location
      "09601"
      "El Malecón"
      [Havana]
      T
      [Spade, Squiggle, Equals]
      DancingMad

granTeatroDeLaHabana :: CardDef
granTeatroDeLaHabana =
  victory 1
    $ location
      "09603"
      "Gran Teatro de la Habana"
      [Havana]
      Square
      [Hourglass, Equals, Squiggle]
      DancingMad

jardinesDeLaTropical :: CardDef
jardinesDeLaTropical =
  victory 1
    $ location
      "09602"
      "Jardines de la Tropical"
      [Havana]
      Squiggle
      [Spade, T, Square]
      DancingMad

miramarYachtClub :: CardDef
miramarYachtClub =
  location
    "09604"
    "Miramar Yacht Club"
    [Havana]
    Equals
    [Hourglass, Square, T]
    DancingMad

plazaHotel :: CardDef
plazaHotel =
  victory 1
    $ location
      "09605"
      "Plaza Hotel"
      [Havana]
      Hourglass
      [Equals, Square]
      DancingMad
