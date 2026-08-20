module Arkham.Enemy.CardDefs.ChildrenOfBlood.PreyedUpon where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

nightFeeder :: CardDef
nightFeeder =
  (enemy "13110" "Night Feeder" PreyedUpon 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdHealth = health 4
    , cdEvade = evade 3
    , cdCardTraits = setFromList [Humanoid, Monster]
    , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter]
    }
