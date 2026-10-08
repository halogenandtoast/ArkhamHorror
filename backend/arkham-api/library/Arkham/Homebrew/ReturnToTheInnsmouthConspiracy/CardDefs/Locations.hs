module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations where

import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Set
import Arkham.Location.CardDefs.Import

{- | return_to_flooded_caverns. These share the official Flooded Caverns unrevealed
back, so they can be shuffled in among the originals: one of each replaces its
counterpart from Flooded Caverns, leaving six unique Tidal Tunnels.
-}
underwaterCavern :: CardDef
underwaterCavern =
  quantity 2
    $ locationWithUnrevealed
      ":return-to-the-innsmouth-conspiracy:065"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      ("Underwater Cavern" <:> "Submerged Nexus")
      [Cave]
      NoSymbol
      []
      Set.ReturnToFloodedCaverns

tidalPool :: CardDef
tidalPool =
  quantity 2
    $ locationWithUnrevealed
      ":return-to-the-innsmouth-conspiracy:066"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      ("Tidal Pool" <:> "Gathering Place")
      [Cave]
      NoSymbol
      []
      Set.ReturnToFloodedCaverns

undergroundRiver :: CardDef
undergroundRiver =
  victory 1
    $ quantity 2
    $ locationWithUnrevealed
      ":return-to-the-innsmouth-conspiracy:067"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      ("Underground River" <:> "Strong Currents")
      [Cave]
      NoSymbol
      []
      Set.ReturnToFloodedCaverns

-- | return_to_devil_reef. A concealed Devil Reef location, so it shares that back.
caveMouth :: CardDef
caveMouth =
  locationWithUnrevealed
    ":return-to-the-innsmouth-conspiracy:033"
    "Devil Reef"
    [Ocean, Island]
    Circle
    [Triangle]
    "Cave Mouth"
    [Ocean, Island, Cave]
    Circle
    [Triangle]
    Set.ReturnToDevilReef

-- return_to_horror_in_high_gear. Road locations, so they share the Innsmouth Road back.

mudTracks :: CardDef
mudTracks =
  locationWithUnrevealed
    ":return-to-the-innsmouth-conspiracy:036"
    "Old Innsmouth Road"
    [Road]
    NoSymbol
    []
    "Mud Tracks"
    [Road]
    NoSymbol
    []
    Set.ReturnToHorrorInHighGear

straightSection :: CardDef
straightSection =
  locationWithUnrevealed
    ":return-to-the-innsmouth-conspiracy:037"
    "Old Innsmouth Road"
    [Road]
    NoSymbol
    []
    "Straight Section"
    [Road]
    NoSymbol
    []
    Set.ReturnToHorrorInHighGear

-- | return_to_devil_reef. Shuffled into the encounter deck, so it has an encounter back.

{- | Drawn from the encounter deck, so it needs 'singleSided': 'location' marks a card
double-sided, which both routes it out of the encounter deck when the set is gathered and
has 'shuffleEncounterDeck' filter it back out again.
-}
shrineToHydra :: CardDef
shrineToHydra =
  ( singleSided
      $ location
        ":return-to-the-innsmouth-conspiracy:032"
        "Shrine to Hydra"
        [Cave]
        NoSymbol
        [Diamond]
        Set.ReturnToDevilReef
  )
    { cdVictoryPoints = Just 1
    }

-- return_to_the_lair_of_dagon. Both share the Tidal Tunnel back.

doorwayToTheDepthsV2 :: CardDef
doorwayToTheDepthsV2 =
  locationWithUnrevealed
    ":return-to-the-innsmouth-conspiracy:044"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    ("Doorway to the Depths" <:> "True Believer's Den")
    [Cave]
    Circle
    [Diamond]
    Set.ReturnToTheLairOfDagon

doorwayToTheDepthsV3 :: CardDef
doorwayToTheDepthsV3 =
  locationWithUnrevealed
    ":return-to-the-innsmouth-conspiracy:045"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    ("Doorway to the Depths" <:> "Secret Passage")
    [Cave]
    Circle
    [Diamond]
    Set.ReturnToTheLairOfDagon
