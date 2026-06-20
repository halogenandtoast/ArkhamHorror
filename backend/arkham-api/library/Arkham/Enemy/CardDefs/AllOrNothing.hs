module Arkham.Enemy.CardDefs.AllOrNothing where

import Arkham.Enemy.CardDefs.Import

siobhanRiley :: CardDef
siobhanRiley =
  unique
    $ (enemy "90015" ("Siobhan Riley" <:> "O'Bannion Enforcer") AllOrNothing 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 5
      , cdEvade = evade 2
      , cdHealth = health 6
      , cdCardTraits = setFromList [Humanoid, Criminal, Elite]
      , cdVictoryPoints = Just 1
      }

cloverClubBouncer :: CardDef
cloverClubBouncer =
  (enemy "90016" "Clover Club Bouncer" AllOrNothing 4)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Criminal]
    , cdVictoryPoints = Just 0
    }
