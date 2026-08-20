module Arkham.Enemy.CardDefs.TheForgottenAge.YigsVenom where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

fangOfYig :: CardDef
fangOfYig =
  (enemy "04098" "Fang of Yig" YigsVenom 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdKeywords = singleton Keyword.Retaliate
    }
