module Arkham.Location.CardDefs.GuardiansOfTheAbyss.TheNightsUsurper where

import Arkham.Location.CardDefs.Import

aDreamBetwixt :: CardDef
aDreamBetwixt =
  otherSideIs "83022b"
    $ location "83022a" "A Dream Betwixt" [Otherworld, Extradimensional] Equals [Square] TheNightsUsurper

eldritchGate :: CardDef
eldritchGate =
  singleSided
    $ location
      "83028"
      "Eldritch Gate"
      [Expedition, Desert, Ruins]
      Squiggle
      [Hourglass, T, Heart]
      TheNightsUsurper

mistFilledCaverns :: CardDef
mistFilledCaverns =
  otherSideIs "83026b"
    $ location "83026a" "Mist-Filled Caverns" [Otherworld, Dreamlands] Circle [Square] TheNightsUsurper

stairwayToSarkomand :: CardDef
stairwayToSarkomand =
  otherSideIs "83025b"
    $ location "83025a" "Stairway to Sarkomand" [Otherworld, Dreamlands] Diamond [Square] TheNightsUsurper

theGreatAbyss :: CardDef
theGreatAbyss =
  victory 1
    $ otherSideIs "83023b"
    $ location
      "83023a"
      "The Great Abyss"
      [Otherworld, Dreamlands]
      Square
      [Equals, Triangle, Diamond, Circle]
      TheNightsUsurper

tunnelsUnderNgranek :: CardDef
tunnelsUnderNgranek =
  otherSideIs "83024b"
    $ location
      "83024a"
      "Tunnels under Ngranek"
      [Otherworld, Dreamlands]
      Triangle
      [Square]
      TheNightsUsurper
