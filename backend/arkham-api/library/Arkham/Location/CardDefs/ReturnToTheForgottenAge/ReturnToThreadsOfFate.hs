module Arkham.Location.CardDefs.ReturnToTheForgottenAge.ReturnToThreadsOfFate where

import Arkham.Location.CardDefs.Import

loadingDocks :: CardDef
loadingDocks =
  victory 1
    $ otherSideIs "53034"
    $ location
      "53034b"
      "Loading Docks"
      [Arkham]
      Squiggle
      [Circle]
      ReturnToThreadsOfFate

theHastingsEstate :: CardDef
theHastingsEstate =
  victory 1
    $ otherSideIs "53029"
    $ location
      "53029b"
      "The Hastings Estate"
      [Arkham]
      Square
      [Diamond, Circle]
      ReturnToThreadsOfFate
