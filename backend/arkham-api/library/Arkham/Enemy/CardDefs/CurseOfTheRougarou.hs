module Arkham.Enemy.CardDefs.CurseOfTheRougarou where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

bogGator :: CardDef
bogGator =
  (enemy "81022" "Bog Gator" TheBayou 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Creature]
    }

darkYoungHost :: CardDef
darkYoungHost =
  (enemy "81033" "Dark Young Host" CurseOfTheRougarou 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, DarkYoung]
    , cdVictoryPoints = Just 1
    }

marshGug :: CardDef
marshGug =
  (enemy "81032" "Marsh Gug" CurseOfTheRougarou 2)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Gug]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

slimeCoveredDhole :: CardDef
slimeCoveredDhole =
  (enemy "81031" "Slime-Covered Dhole" CurseOfTheRougarou 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Dhole]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

swampLeech :: CardDef
swampLeech =
  (enemy "81023" "Swamp Leech" TheBayou 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 4
    , cdHealth = health 1
    , cdCardTraits = setFromList [Creature]
    }

theRougarou :: CardDef
theRougarou =
  unique
    $ (enemy "81028" ("The Rougarou" <:> "Cursed Soul") CurseOfTheRougarou 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Monster, Creature, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
      }
