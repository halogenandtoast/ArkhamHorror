module Arkham.Enemy.CardDefs.ReturnToThePathToCarcosa.ReturnToEchoesOfThePast where

import Arkham.Enemy.CardDefs.Import

keeperOfTheOath :: CardDef
keeperOfTheOath =
  (enemy "52033" "Keeper of the Oath" ReturnToEchoesOfThePast 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdVictoryPoints = Just 0
    }
