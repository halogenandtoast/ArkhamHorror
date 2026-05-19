{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.Core2026 where

import Arkham.Location.CardDefs.Import
import Arkham.EncounterSet qualified as Set

yourFriendsRoom :: CardDef
yourFriendsRoom =
  location
    "12113"
    "Your Friend's Room"
    [Miskatonic]
    Spade
    [Triangle]
    SpreadingFlames

miskatonicQuad_MiskatonicUniversity :: CardDef
miskatonicQuad_MiskatonicUniversity =
  location
    "12116"
    "Miskatonic Quad"
    [Miskatonic, Central]
    Plus
    [Triangle, Hourglass, Circle, Heart, Square, Diamond]
    MiskatonicUniversity

dormitories_MiskatonicUniversity :: CardDef
dormitories_MiskatonicUniversity =
  location
    "12117"
    "Dormitories"
    [Miskatonic]
    Triangle
    [Spade, Plus]
    MiskatonicUniversity

scienceHall :: CardDef
scienceHall =
  victory 1
    $ location
      "12118"
      "Science Hall"
      [Miskatonic]
      Hourglass
      [Plus]
      MiskatonicUniversity

warrenObservatory_MiskatonicUniversity :: CardDef
warrenObservatory_MiskatonicUniversity =
  location
    "12119"
    "Warren Observatory"
    [Miskatonic]
    Square
    [Plus]
    MiskatonicUniversity

orneLibrary_MiskatonicUniversity :: CardDef
orneLibrary_MiskatonicUniversity =
  victory 1
    $ location
      "12120"
      "Orne Library"
      [Miskatonic]
      Diamond
      [Plus]
      MiskatonicUniversity

downtownFirstBankOfArkham_Arkham :: CardDef
downtownFirstBankOfArkham_Arkham =
  victory 1
    $ location
      "12145"
      ("Downtown" <:> "First Bank of Arkham")
      [Arkham]
      Triangle
      [Moon, T]
      Set.Arkham

downtownArkhamSanatorium :: CardDef
downtownArkhamSanatorium =
  victory 1
    $ location
      "12146"
      ("Downtown" <:> "Arkham Sanatorium")
      [Arkham]
      Triangle
      [Moon, T]
      Set.Arkham

uptownStMarysHospital :: CardDef
uptownStMarysHospital =
  location
    "12147"
    ("Uptown" <:> "St. Mary's Hospital")
    [Arkham]
    Plus
    [Diamond, Square]
    Set.Arkham

uptownYeOldeMagickShoppe :: CardDef
uptownYeOldeMagickShoppe =
  location
    "12148"
    ("Uptown" <:> "Ye Olde Magick Shoppe")
    [Arkham]
    Plus
    [Diamond, Square]
    Set.Arkham

northside_Arkham :: CardDef
northside_Arkham =
  location
    "12149"
    "Northside"
    [Arkham]
    T
    [Diamond, Triangle]
    Set.Arkham

easttown_Arkham :: CardDef
easttown_Arkham =
  location
    "12150"
    "Easttown"
    [Arkham]
    Moon
    [Circle, Triangle, Spade]
    Set.Arkham

merchantDistrict_Arkham :: CardDef
merchantDistrict_Arkham =
  location
    "12151"
    "Merchant District"
    [Arkham]
    Circle
    [Moon, Diamond, Square, Spade]
    Set.Arkham

waterfrontDistrict :: CardDef
waterfrontDistrict =
  victory 1
    $ location
      "12152"
      "Waterfront District"
      [Arkham]
      Spade
      [Circle, Moon]
      Set.Arkham

southside_Arkham :: CardDef
southside_Arkham =
  location
    "12153"
    "Southside"
    [Arkham]
    Square
    [Plus, Hourglass, Circle]
    Set.Arkham

frenchHill_Arkham :: CardDef
frenchHill_Arkham =
  victory 1
    $ location
      "12154"
      "French Hill"
      [Arkham]
      Hourglass
      [Square]
      Set.Arkham

miskatonicUniversityInFlames :: CardDef
miskatonicUniversityInFlames =
  victory 1
    $ location
      "12155"
      ("Miskatonic University" <:> "In Flames")
      [Arkham]
      Diamond
      [T, Plus, Circle]
      Set.Arkham

miskatonicUniversityQuietCampus :: CardDef
miskatonicUniversityQuietCampus =
  victory 1
    $ location
      "12156"
      ("Miskatonic University" <:> "Quiet Campus")
      [Arkham]
      Diamond
      [T, Plus, Circle]
      Set.Arkham

sewerCulvert :: CardDef
sewerCulvert =
  location
    "12182"
    "Sewer Culvert"
    [Sewer, Central]
    Hourglass
    [Triangle]
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

undergroundCistern :: CardDef
undergroundCistern =
  location
    "12174"
    "Underground Cistern"
    [RitualSite]
    Circle
    [Triangle, Plus, Spade, Star, Equals]
    QueenOfAsh

sluiceControl :: CardDef
sluiceControl =
  location
    "12175"
    "Sluice Control"
    [Sewer]
    Plus
    [Circle]
    QueenOfAsh
