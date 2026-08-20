module Arkham.Location.CardDefs.BrethrenOfAsh.QueenOfAsh where

import Arkham.Location.CardDefs.Import

sewerCulvert :: CardDef
sewerCulvert =
  location
    "12182"
    "Sewer Culvert"
    [Sewer, Central]
    Hourglass
    [Triangle]
    Sewers

sewerTunnelsFloodedCrypt :: CardDef
sewerTunnelsFloodedCrypt =
  victory 1
    $ locationWithUnrevealed
      "12185"
      "Sewer Tunnels"
      [Sewer]
      Triangle
      [Hourglass, Circle]
      ("Sewer Tunnels" <:> "Flooded Crypt")
      [Sewer]
      Star
      [Hourglass, Circle]
      Sewers

sewerTunnelsInfestedPipes :: CardDef
sewerTunnelsInfestedPipes =
  locationWithUnrevealed
    "12183"
    "Sewer Tunnels"
    [Sewer]
    Triangle
    [Hourglass, Circle]
    ("Sewer Tunnels" <:> "InfestedPipes")
    [Sewer]
    Spade
    [Hourglass, Circle]
    Sewers

sewerTunnelsOvergrownTunnels :: CardDef
sewerTunnelsOvergrownTunnels =
  locationWithUnrevealed
    "12184"
    "Sewer Tunnels"
    [Sewer]
    Triangle
    [Hourglass, Circle]
    ("Sewer Tunnels" <:> "Overgrown Tunnels")
    [Sewer]
    Square
    [Hourglass]
    Sewers

sewerTunnelsSmugglersCache :: CardDef
sewerTunnelsSmugglersCache =
  locationWithUnrevealed
    "12186"
    "Sewer Tunnels"
    [Sewer]
    Triangle
    [Hourglass, Circle]
    ("Sewer Tunnels" <:> "Smugglers Cache")
    [Sewer]
    Trefoil
    [Hourglass]
    Sewers

sewerTunnelsToxicWastePit :: CardDef
sewerTunnelsToxicWastePit =
  victory 1
    $ locationWithUnrevealed
      "12187"
      "Sewer Tunnels"
      [Sewer]
      Triangle
      [Hourglass, Circle]
      ("Sewer Tunnels" <:> "Toxic Waste Pit")
      [Sewer]
      Equals
      [Hourglass, Circle]
      Sewers

sluiceControl :: CardDef
sluiceControl =
  location
    "12175"
    "Sluice Control"
    [Sewer]
    Plus
    [Circle]
    QueenOfAsh

undergroundCistern :: CardDef
undergroundCistern =
  location
    "12174"
    "Underground Cistern"
    [RitualSite]
    Circle
    [Triangle, Plus, Spade, Star, Equals]
    QueenOfAsh
