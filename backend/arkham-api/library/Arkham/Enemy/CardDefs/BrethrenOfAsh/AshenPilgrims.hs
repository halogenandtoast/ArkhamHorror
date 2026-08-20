module Arkham.Enemy.CardDefs.BrethrenOfAsh.AshenPilgrims where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

cantorOfFlame :: CardDef
cantorOfFlame =
  (enemy "12121" "Cantor of Flame" AshenPilgrims 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

hellhound :: CardDef
hellhound =
  (enemy "12122" "Hellhound" AshenPilgrims 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Monster]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
