module Arkham.Enemy.CardDefs.ReturnToThePathToCarcosa.ReturnToThePallidMask where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

malformedSkeleton :: CardDef
malformedSkeleton =
  (enemy "52053" "Malformed Skeleton" ReturnToThePallidMask 1)
    { cdHealthDamage = healthDamage 3
    , cdSanityDamage = sanityDamage 3
    , cdFight = fight 4
    , cdEvade = evade 1
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
