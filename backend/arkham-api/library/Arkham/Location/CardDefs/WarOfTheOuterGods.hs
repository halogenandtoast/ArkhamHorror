module Arkham.Location.CardDefs.WarOfTheOuterGods where

import Arkham.Location.CardDefs.Import

arkham :: CardDef
arkham =
  location
    "86014"
    "Arkham"
    [RitualSite, Town]
    Triangle
    [Star, Hourglass, Circle]
    WarOfTheOuterGods

athenaeumOfTheEmptySky :: CardDef
athenaeumOfTheEmptySky =
  location
    "86016"
    "Athenaeum of the Empty Sky"
    [RitualSite, Providence]
    Droplet
    [Star, T]
    WarOfTheOuterGods

chateauRamezay :: CardDef
chateauRamezay =
  location "86019" "Chateau Ramezay" [Montreal] Equals [Hourglass, Heart] WarOfTheOuterGods

hubDimension :: CardDef
hubDimension =
  (location "86024" "Hub Dimension" [Portal] NoSymbol [] WarOfTheOuterGods)
    { cdRevealedName = Just ("Hub Dimension" <:> "Gateway to Destruction")
    }

shrineOfMaghanArkat :: CardDef
shrineOfMaghanArkat =
  location
    "86020"
    "Shrine of Magh'an Ark'at"
    [RitualSite, Montreal]
    Heart
    [Hourglass, Equals]
    WarOfTheOuterGods

streetsOfMontreal :: CardDef
streetsOfMontreal =
  victory 1
    $ location
      "86018"
      "Streets of Montréal"
      [Montreal]
      Hourglass
      [Triangle, Equals, Heart]
      WarOfTheOuterGods

streetsOfNewYorkCity :: CardDef
streetsOfNewYorkCity =
  victory 1
    $ location
      "86021"
      "Streets of New York City"
      [NewYorkCity]
      Circle
      [Triangle, Moon, Spade]
      WarOfTheOuterGods

streetsOfProvidence :: CardDef
streetsOfProvidence =
  victory 1
    $ location
      "86015"
      "Streets of Providence"
      [Providence]
      Star
      [Triangle, T, Droplet]
      WarOfTheOuterGods

theArcade :: CardDef
theArcade = location "86017" "The Arcade" [Providence] T [Star, Droplet] WarOfTheOuterGods

theBurningPit :: CardDef
theBurningPit =
  location "86022" "The Burning Pit" [RitualSite, NewYorkCity] Moon [Circle, Spade] WarOfTheOuterGods

thePenthouse :: CardDef
thePenthouse =
  location "86023" "The Penthouse" [NewYorkCity] Spade [Circle, Moon] WarOfTheOuterGods
