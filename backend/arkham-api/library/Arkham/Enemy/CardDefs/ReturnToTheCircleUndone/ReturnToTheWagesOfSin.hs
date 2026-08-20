{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.CardDefs.ReturnToTheCircleUndone.ReturnToTheWagesOfSin where

import Arkham.Enemy.CardDefs.Import

returnToHeretic_38 :: CardDef
returnToHeretic_38 =
  doubleSided "54038b"
    $ (enemy "54038" "Heretic" ReturnToTheWagesOfSin 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
      }

returnToHeretic_39 :: CardDef
returnToHeretic_39 =
  doubleSided "54039b"
    $ (enemy "54039" "Heretic" ReturnToTheWagesOfSin 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
      }
