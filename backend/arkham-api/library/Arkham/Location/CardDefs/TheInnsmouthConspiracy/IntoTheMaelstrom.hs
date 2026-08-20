module Arkham.Location.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom where

import Arkham.Location.CardDefs.Import

darkAbyss :: CardDef
darkAbyss =
  quantity 2
    $ locationWithUnrevealed
      "07319"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      "Dark Abyss"
      [Cave]
      NoSymbol
      []
      IntoTheMaelstrom

gatewayToYhanthlei :: CardDef
gatewayToYhanthlei =
  location
    "07320"
    "Gateway to Y'ha-nthlei"
    [Yhanthlei, Otherworld]
    NoSymbol
    []
    IntoTheMaelstrom

lairOfDagon :: CardDef
lairOfDagon =
  location
    "07328"
    ("Lair of Dagon" <:> "Sanctuary of Father Dagon")
    [Yhanthlei, Lair]
    NoSymbol
    []
    IntoTheMaelstrom

lairOfHydra :: CardDef
lairOfHydra =
  location
    "07329"
    ("Lair of Hydra" <:> "High Temple of Mother Hydra")
    [Yhanthlei, Lair]
    NoSymbol
    []
    IntoTheMaelstrom

onyxGuardians :: CardDef
onyxGuardians =
  locationWithUnrevealed
    "07327"
    "Y'ha-nthlei Sanctum"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    "Onyx Guardians"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    IntoTheMaelstrom

statuesInTheDeep :: CardDef
statuesInTheDeep =
  locationWithUnrevealed
    "07324"
    "Y'ha-nthlei Sanctum"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    "Statues in the Deep"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    IntoTheMaelstrom

submergedTemple :: CardDef
submergedTemple =
  locationWithUnrevealed
    "07325"
    "Y'ha-nthlei Sanctum"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    "Submerged Temple"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    IntoTheMaelstrom

sunkenHalls :: CardDef
sunkenHalls =
  quantity 2
    $ locationWithUnrevealed
      "07321"
      "Y'ha-nthlei"
      [Yhanthlei]
      NoSymbol
      []
      "Sunken Halls"
      [Yhanthlei]
      NoSymbol
      []
      IntoTheMaelstrom

syzygyChamber :: CardDef
syzygyChamber =
  locationWithUnrevealed
    "07326"
    "Y'ha-nthlei Sanctum"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    "Syzygy Chamber"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    IntoTheMaelstrom

underseaCorridors :: CardDef
underseaCorridors =
  quantity 3
    $ locationWithUnrevealed
      "07323"
      "Y'ha-nthlei"
      [Yhanthlei]
      NoSymbol
      []
      "Undersea Corridors"
      [Yhanthlei]
      NoSymbol
      []
      IntoTheMaelstrom

vaultOfRiches :: CardDef
vaultOfRiches =
  quantity 2
    $ locationWithUnrevealed
      "07322"
      "Y'ha-nthlei"
      [Yhanthlei]
      NoSymbol
      []
      "Vault of Riches"
      [Yhanthlei]
      NoSymbol
      []
      IntoTheMaelstrom
