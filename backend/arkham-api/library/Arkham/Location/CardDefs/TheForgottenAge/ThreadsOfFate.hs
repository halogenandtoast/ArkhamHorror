module Arkham.Location.CardDefs.TheForgottenAge.ThreadsOfFate where

import Arkham.Location.CardDefs.Import

arkhamPoliceStation :: CardDef
arkhamPoliceStation =
  victory 1
    $ otherSideIs "04126"
    $ location
      "04126b"
      "Arkham Police Station"
      [Arkham]
      NoSymbol
      [Moon]
      ThreadsOfFate

blackCave :: CardDef
blackCave =
  victory 1
    $ otherSideIs "04133"
    $ location
      "04133b"
      "Black Cave"
      [Cave]
      Hourglass
      [Circle]
      ThreadsOfFate

curiositieShoppe :: CardDef
curiositieShoppe =
  victory 1
    $ location "04142" "Curiositie Shoppe" [Arkham] NoSymbol [T] ThreadsOfFate

eztliExhibit :: CardDef
eztliExhibit =
  victory 1
    $ otherSideIs "04117"
    $ location
      "04117b"
      "Eztli Exhibit"
      [Miskatonic, Exhibit]
      Plus
      [Diamond]
      ThreadsOfFate

townHall :: CardDef
townHall =
  victory 1
    $ location "04143" "Town Hall" [Arkham] NoSymbol [Triangle] ThreadsOfFate

trainTracks :: CardDef
trainTracks =
  otherSideIs "04128"
    $ location "04128b" "Train Tracks" [Arkham] NoSymbol [T] ThreadsOfFate

velmasDiner :: CardDef
velmasDiner =
  location "04141" "Velma's Diner" [Arkham] NoSymbol [Moon] ThreadsOfFate
