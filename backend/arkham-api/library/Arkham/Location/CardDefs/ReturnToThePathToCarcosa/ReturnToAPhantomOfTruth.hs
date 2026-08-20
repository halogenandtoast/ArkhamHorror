module Arkham.Location.CardDefs.ReturnToThePathToCarcosa.ReturnToAPhantomOfTruth where

import Arkham.Location.CardDefs.Import

returnToCanalSaintMartin :: CardDef
returnToCanalSaintMartin =
  victory 1
    $ location "52044" "Canal Saint-Martin" [Paris] Equals [Square, T, Moon] ReturnToAPhantomOfTruth

returnToGardensOfLuxembourg :: CardDef
returnToGardensOfLuxembourg =
  victory 1
    $ location "52046" "Gardens of Luxembourg" [Paris] Star [Circle, Heart, Plus] ReturnToAPhantomOfTruth

returnToGrandGuignol :: CardDef
returnToGrandGuignol =
  victory 1
    $ location
      "52042"
      ("Grand Guignol" <:> "Theatre of the Great Puppet")
      [Paris]
      Triangle
      [Diamond, Square]
      ReturnToAPhantomOfTruth

returnToMontparnasse :: CardDef
returnToMontparnasse =
  location "52041" "Montparnasse" [Paris, Rail] Circle [Heart, Star, Plus] ReturnToAPhantomOfTruth

returnToNotreDame :: CardDef
returnToNotreDame = location "52045" "Notre-Dame" [Paris, Rail] Plus [Circle, Moon, Star] ReturnToAPhantomOfTruth

returnToPereLachaiseCemetery :: CardDef
returnToPereLachaiseCemetery =
  victory 1
    $ location "52043" "Père Lachaise Cemetery" [Paris] T [Equals, Moon] ReturnToAPhantomOfTruth
