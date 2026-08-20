module Arkham.Enemy.CardDefs.TheForgottenAge where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

serpentsOfYig :: CardDef
serpentsOfYig =
  (weakness "04014" "Serpents of Yig")
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdKeywords = singleton Keyword.Hunter
    , cdRevelation = IsRevelation
    }
