module Arkham.Enemy.CardDefs.TheScarletKeys.CleanupCrew where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

coterieAssassinA :: CardDef
coterieAssassinA =
  (enemy "09727a" "Coterie Assassin (A)" CleanupCrew 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords = singleton $ Keyword.Concealed CoterieAssassinA (Static 1)
    }

coterieAssassinB :: CardDef
coterieAssassinB =
  (enemy "09727b" "Coterie Assassin (B)" CleanupCrew 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords = singleton $ Keyword.Concealed CoterieAssassinB (Static 1)
    }

coterieEnforcerA :: CardDef
coterieEnforcerA =
  (enemy "09726a" "Coterie Enforcer (A)" CleanupCrew 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords = singleton $ Keyword.Concealed CoterieEnforcerA (Static 1)
    }

coterieEnforcerB :: CardDef
coterieEnforcerB =
  (enemy "09726b" "Coterie Enforcer (B)" CleanupCrew 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords = singleton $ Keyword.Concealed CoterieEnforcerB (Static 1)
    }
