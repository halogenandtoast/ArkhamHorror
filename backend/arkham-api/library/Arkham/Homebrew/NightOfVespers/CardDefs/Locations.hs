module Arkham.Homebrew.NightOfVespers.CardDefs.Locations where

import Arkham.Homebrew.NightOfVespers.Sets qualified as Set
import Arkham.Location.CardDefs.Import

trattoriaDelParcheLaDolceVita :: CardDef
trattoriaDelParcheLaDolceVita =
  otherSideIs ":night-of-vespers:008b"
    $ location
      ":night-of-vespers:008"
      ("Trattoria del Parche" <:> "La Dolce Vita")
      [HomebrewTrait "Hotel", HomebrewTrait "Restaurant"]
      NoSymbol
      []
      Set.TheEndOfSeasons

trattoriaDelParcheLaBruttaVita :: CardDef
trattoriaDelParcheLaBruttaVita =
  otherSideIs ":night-of-vespers:008"
    $ location
      ":night-of-vespers:008b"
      ("Trattoria del Parche" <:> "La Brutta Vita")
      [HomebrewTrait "Hotel", HomebrewTrait "Restaurant"]
      NoSymbol
      []
      Set.TheEndOfSeasons

conservatory :: CardDef
conservatory =
  location
    ":night-of-vespers:009"
    "Conservatory"
    [HomebrewTrait "Hotel"]
    NoSymbol
    []
    Set.TheEndOfSeasons

hotelRooftop :: CardDef
hotelRooftop =
  location
    ":night-of-vespers:010"
    "Hotel Rooftop"
    [HomebrewTrait "Hotel"]
    NoSymbol
    []
    Set.TheEndOfSeasons

hedgeMaze :: CardDef
hedgeMaze =
  location
    ":night-of-vespers:011"
    "Hedge Maze"
    [HomebrewTrait "Hotel"]
    NoSymbol
    []
    Set.TheEndOfSeasons

grandBallroom :: CardDef
grandBallroom =
  victory 2
    $ location
      ":night-of-vespers:012"
      "Grand Ballroom"
      [HomebrewTrait "Hotel"]
      NoSymbol
      []
      Set.TheEndOfSeasons

barLAmmaliatrice_013 :: CardDef
barLAmmaliatrice_013 =
  location
    ":night-of-vespers:013"
    "Bar L'Ammaliatrice"
    [HomebrewTrait "Hotel"]
    NoSymbol
    []
    Set.TheEndOfSeasons

barLAmmaliatrice_014 :: CardDef
barLAmmaliatrice_014 =
  location
    ":night-of-vespers:014"
    "Bar L'Ammaliatrice"
    [HomebrewTrait "Hotel"]
    NoSymbol
    []
    Set.TheEndOfSeasons

galerieLavandiere_015 :: CardDef
galerieLavandiere_015 =
  location
    ":night-of-vespers:015"
    ("Galerie Lavandière" <:> "Fine Art Gallery")
    [HomebrewTrait "Hotel"]
    NoSymbol
    []
    Set.TheEndOfSeasons

galerieLavandiere_016 :: CardDef
galerieLavandiere_016 =
  location
    ":night-of-vespers:016"
    ("Galerie Lavandière" <:> "Fine Art Gallery")
    [HomebrewTrait "Hotel"]
    NoSymbol
    []
    Set.TheEndOfSeasons

trattoriaKitchens_017 :: CardDef
trattoriaKitchens_017 =
  location
    ":night-of-vespers:017"
    "Trattoria Kitchens"
    [HomebrewTrait "Hotel", HomebrewTrait "Restaurant"]
    NoSymbol
    []
    Set.TheEndOfSeasons

trattoriaKitchens_018 :: CardDef
trattoriaKitchens_018 =
  location
    ":night-of-vespers:018"
    "Trattoria Kitchens"
    [HomebrewTrait "Hotel", HomebrewTrait "Restaurant"]
    NoSymbol
    []
    Set.TheEndOfSeasons

hotelLobby_019 :: CardDef
hotelLobby_019 =
  victory 2
    $ location
      ":night-of-vespers:019"
      "Hotel Lobby"
      [HomebrewTrait "Hotel"]
      NoSymbol
      []
      Set.TheEndOfSeasons

hotelLobby_020 :: CardDef
hotelLobby_020 =
  victory 2
    $ location
      ":night-of-vespers:020"
      "Hotel Lobby"
      [HomebrewTrait "Hotel"]
      NoSymbol
      []
      Set.TheEndOfSeasons

luxurySuites_021 :: CardDef
luxurySuites_021 =
  victory 2
    $ location
      ":night-of-vespers:021"
      "Luxury Suites"
      [HomebrewTrait "Hotel"]
      NoSymbol
      []
      Set.TheEndOfSeasons

luxurySuites_022 :: CardDef
luxurySuites_022 =
  victory 2
    $ location
      ":night-of-vespers:022"
      "Luxury Suites"
      [HomebrewTrait "Hotel"]
      NoSymbol
      []
      Set.TheEndOfSeasons

mezzanineElevators_023 :: CardDef
mezzanineElevators_023 =
  location
    ":night-of-vespers:023"
    "Mezzanine Elevators"
    [HomebrewTrait "Hotel"]
    NoSymbol
    []
    Set.TheEndOfSeasons

mezzanineElevators_024 :: CardDef
mezzanineElevators_024 =
  location
    ":night-of-vespers:024"
    "Mezzanine Elevators"
    [HomebrewTrait "Hotel"]
    NoSymbol
    []
    Set.TheEndOfSeasons

pleasureGardens_025 :: CardDef
pleasureGardens_025 =
  victory 2
    $ location
      ":night-of-vespers:025"
      "Pleasure Gardens"
      [HomebrewTrait "Hotel"]
      NoSymbol
      []
      Set.TheEndOfSeasons

pleasureGardens_026 :: CardDef
pleasureGardens_026 =
  victory 2
    $ location
      ":night-of-vespers:026"
      "Pleasure Gardens"
      [HomebrewTrait "Hotel"]
      NoSymbol
      []
      Set.TheEndOfSeasons

presidentialSuite_027 :: CardDef
presidentialSuite_027 =
  victory 2
    $ location
      ":night-of-vespers:027"
      "Presidential Suite"
      [HomebrewTrait "Hotel"]
      NoSymbol
      []
      Set.TheEndOfSeasons

presidentialSuite_028 :: CardDef
presidentialSuite_028 =
  victory 2
    $ location
      ":night-of-vespers:028"
      "Presidential Suite"
      [HomebrewTrait "Hotel"]
      NoSymbol
      []
      Set.TheEndOfSeasons

hotelWineCellar_029 :: CardDef
hotelWineCellar_029 =
  location
    ":night-of-vespers:029"
    "Hotel Wine Cellar"
    [HomebrewTrait "Hotel"]
    NoSymbol
    []
    Set.TheEndOfSeasons

hotelWineCellar_030 :: CardDef
hotelWineCellar_030 =
  location
    ":night-of-vespers:030"
    "Hotel Wine Cellar"
    [HomebrewTrait "Hotel"]
    NoSymbol
    []
    Set.TheEndOfSeasons
