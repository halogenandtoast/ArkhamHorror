module Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.CreaturesOfTheDeep where

import Arkham.Enemy.CardDefs.Import

deepOneBull :: CardDef
deepOneBull =
  (enemy "07088" "Deep One Bull" CreaturesOfTheDeep 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    }

lurkingDeepOne :: CardDef
lurkingDeepOne =
  (enemy "07089" "Lurking Deep One" CreaturesOfTheDeep 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    }
