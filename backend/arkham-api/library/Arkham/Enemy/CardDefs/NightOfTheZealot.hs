module Arkham.Enemy.CardDefs.NightOfTheZealot where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

mobEnforcer :: CardDef
mobEnforcer =
  (basicWeakness "01101" "Mob Enforcer")
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Criminal]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdAlternateCardCodes = ["01601"]
    }

silverTwilightAcolyte :: CardDef
silverTwilightAcolyte =
  (basicWeakness "01102" "Silver Twilight Acolyte")
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdAlternateCardCodes = ["01602"]
    }

stubbornDetective :: CardDef
stubbornDetective =
  (basicWeakness "01103" "Stubborn Detective")
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Detective]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdAlternateCardCodes = ["01603"]
    }
