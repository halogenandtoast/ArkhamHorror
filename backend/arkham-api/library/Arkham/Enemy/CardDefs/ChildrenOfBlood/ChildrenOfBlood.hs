module Arkham.Enemy.CardDefs.ChildrenOfBlood.ChildrenOfBlood where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

childOfBlood :: CardDef
childOfBlood =
  (enemy "13103" "Child of Blood" ChildrenOfBlood 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdHealth = health 2
    , cdEvade = evade 3
    , cdCardTraits = setFromList [Humanoid, Monster]
    , cdKeywords = singleton Keyword.Hunter
    }
