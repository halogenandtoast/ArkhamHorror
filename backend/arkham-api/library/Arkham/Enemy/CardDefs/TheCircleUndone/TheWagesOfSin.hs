{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.CardDefs.TheCircleUndone.TheWagesOfSin where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

heretic_A :: CardDef
heretic_A =
  doubleSided "05178b"
    $ (enemy "05178a" "Heretic" TheWagesOfSin 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
      }

heretic_C :: CardDef
heretic_C =
  doubleSided "05178d"
    $ (enemy "05178c" "Heretic" TheWagesOfSin 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
      }

heretic_E :: CardDef
heretic_E =
  doubleSided "05178f"
    $ (enemy "05178e" "Heretic" TheWagesOfSin 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
      }

heretic_G :: CardDef
heretic_G =
  doubleSided "05178h"
    $ (enemy "05178g" "Heretic" TheWagesOfSin 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
      , cdOutOfPlayEffects = [InDiscardEffect]
      }

heretic_I :: CardDef
heretic_I =
  doubleSided "05178j"
    $ (enemy "05178i" "Heretic" TheWagesOfSin 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
      }

heretic_K :: CardDef
heretic_K =
  doubleSided "05178l"
    $ (enemy "05178k" "Heretic" TheWagesOfSin 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
      }

malevolentSpirit :: CardDef
malevolentSpirit =
  (enemy "05180" "Malevolent Spirit" TheWagesOfSin 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Geist, Spectral]
    }

vengefulWitch :: CardDef
vengefulWitch =
  (enemy "05179" "Vengeful Witch" TheWagesOfSin 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Witch]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    }
