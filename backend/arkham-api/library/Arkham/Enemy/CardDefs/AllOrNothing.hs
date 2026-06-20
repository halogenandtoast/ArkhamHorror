module Arkham.Enemy.CardDefs.AllOrNothing where

import Arkham.Enemy.CardDefs.Import

siobhanRiley :: CardDef
siobhanRiley =
  unique
    $ (enemy "90015" ("Siobhan Riley" <:> "O'Bannion Enforcer") AllOrNothing 1)
      { cdHealth = health 6
      , cdCardTraits = setFromList [Humanoid, Criminal, Elite]
      , cdVictoryPoints = Just 1
      }

cloverClubBouncer :: CardDef
cloverClubBouncer =
  (enemy "90016" "Clover Club Bouncer" AllOrNothing 4)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Criminal]
    , cdVictoryPoints = Just 0
    }
