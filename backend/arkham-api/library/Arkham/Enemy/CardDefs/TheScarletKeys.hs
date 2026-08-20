module Arkham.Enemy.CardDefs.TheScarletKeys where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

agentFletcher :: CardDef
agentFletcher =
  (weakness "09010" "Agent Fletcher")
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie, Detective]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    }

lurkerInTheDark :: CardDef
lurkerInTheDark =
  (basicWeakness "09124" "Lurker in the Dark")
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Shoggoth]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdDeckRestrictions = [OnlyClass Guardian]
    }

ectoplasmicHorror :: CardDef
ectoplasmicHorror =
  (basicWeakness "09127" "Ectoplasmic Horror")
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Geist]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdDeckRestrictions = [OnlyClass Mystic]
    }
