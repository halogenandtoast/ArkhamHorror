module Arkham.Location.CardDefs.GuardiansOfTheAbyss.TheEternalSlumber where

import Arkham.Location.CardDefs.Import

cairoBazaar :: CardDef
cairoBazaar =
  singleSided
    $ location "83009" "Cairo Bazaar" [Cairo] Square [Triangle, Equals, Trefoil] TheEternalSlumber

museumOfEgyptianAntiquities :: CardDef
museumOfEgyptianAntiquities =
  singleSided
    $ location
      "83010"
      "Museum of Egyptian Antiquities"
      [Cairo]
      Triangle
      [Diamond, Equals, Square]
      TheEternalSlumber

outskirtsOfCairo :: CardDef
outskirtsOfCairo =
  singleSided
    $ location
      "83011"
      "Outskirts of Cairo"
      [Cairo]
      Diamond
      [Circle, Triangle, Equals, Trefoil]
      TheEternalSlumber

streetsOfCairo :: CardDef
streetsOfCairo =
  singleSided
    $ location
      "83008"
      "Streets of Cairo"
      [Cairo]
      Equals
      [Diamond, Triangle, Square, Trefoil]
      TheEternalSlumber

templeCourtyard :: CardDef
templeCourtyard =
  singleSided
    $ location "83012" "Temple Courtyard" [Cairo] Trefoil [Diamond, Equals, Square] TheEternalSlumber
