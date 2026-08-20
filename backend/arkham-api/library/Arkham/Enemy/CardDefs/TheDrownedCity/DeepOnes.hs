module Arkham.Enemy.CardDefs.TheDrownedCity.DeepOnes where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

deepOneThrall :: CardDef
deepOneThrall =
  (enemy "11746" "Deep One Thrall" DeepOnes 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    }

elderDeepOne :: CardDef
elderDeepOne =
  (enemy "11747" "Elder Deep One" DeepOnes 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }
