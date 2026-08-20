module Arkham.Treachery.CardDefs.ReturnToTheDunwichLegacy.SecretDoors where

import Arkham.Treachery.CardDefs.Import

secretDoor :: CardDef
secretDoor =
  (treachery "51065" "Secret Door" SecretDoors 2)
    { cdCardTraits = setFromList [Obstacle]
    }
