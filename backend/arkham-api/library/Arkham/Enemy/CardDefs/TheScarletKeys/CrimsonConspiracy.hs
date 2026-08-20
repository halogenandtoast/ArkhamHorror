module Arkham.Enemy.CardDefs.TheScarletKeys.CrimsonConspiracy where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

coterieAgentA :: CardDef
coterieAgentA =
  (enemy "09716a" "Coterie Agent (A)" CrimsonConspiracy 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords = singleton $ Keyword.Concealed CoterieAgentA (Static 2)
    }

coterieAgentB :: CardDef
coterieAgentB =
  (enemy "09716b" "Coterie Agent (B)" CrimsonConspiracy 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords = singleton $ Keyword.Concealed CoterieAgentB (Static 2)
    }

coterieAgentC :: CardDef
coterieAgentC =
  (enemy "09716c" "Coterie Agent (C)" CrimsonConspiracy 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords = singleton $ Keyword.Concealed CoterieAgentC (Static 2)
    }
