module Arkham.Enemy.CardDefs.TheForgottenAge.PnakoticBrotherhood where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

brotherhoodCultist :: CardDef
brotherhoodCultist =
  (enemy "04095" "Brotherhood Cultist" PnakoticBrotherhood 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = singleton Keyword.Hunter
    }
