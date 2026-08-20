module Arkham.Enemy.CardDefs.TheScarletKeys.AgentsOfYuggoth where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

emissaryFromYuggoth :: CardDef
emissaryFromYuggoth =
  (enemy "09739" "Emissary from Yuggoth" AgentsOfYuggoth 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Yuggoth]
    , cdKeywords =
        setFromList [Keyword.Concealed EmissaryFromYuggoth (Static 2), Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    }
