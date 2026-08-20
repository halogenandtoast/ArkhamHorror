module Arkham.Enemy.CardDefs.GuardiansOfTheAbyss.SandsOfEgypt where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

abyssalRevenant :: CardDef
abyssalRevenant =
  (enemy "83045" "Abyssal Revenant" SandsOfEgypt 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Dreamlands]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

thingInTheSarcophagus :: CardDef
thingInTheSarcophagus =
  (enemy "83046" "Thing in the Sarcophagus" SandsOfEgypt 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster]
    }
