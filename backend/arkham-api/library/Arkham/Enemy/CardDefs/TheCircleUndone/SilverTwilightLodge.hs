module Arkham.Enemy.CardDefs.TheCircleUndone.SilverTwilightLodge where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

keeperOfSecrets :: CardDef
keeperOfSecrets =
  (enemy "05096" "Keeper of Secrets" SilverTwilightLodge 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
    }

lodgeNeophyte :: CardDef
lodgeNeophyte =
  (enemy "05095" "Lodge Neophyte" SilverTwilightLodge 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = singleton Keyword.Aloof
    }
