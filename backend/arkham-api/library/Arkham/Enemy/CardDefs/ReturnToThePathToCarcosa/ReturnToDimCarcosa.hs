module Arkham.Enemy.CardDefs.ReturnToThePathToCarcosa.ReturnToDimCarcosa where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

highPriestOfHastur :: CardDef
highPriestOfHastur =
  (enemy "52064" "High Priest of Hastur" ReturnToDimCarcosa 1)
    { cdFight = fight 6
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
