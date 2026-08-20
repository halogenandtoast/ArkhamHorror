module Arkham.Enemy.CardDefs.TheDunwichLegacy.WhereDoomAwaits where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

crazedShoggoth :: CardDef
crazedShoggoth =
  (enemy "02295" "Crazed Shoggoth" WhereDoomAwaits 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 6
    , cdCardTraits = setFromList [Monster, Shoggoth]
    , cdVictoryPoints = Just 1
    }

devoteeOfTheKey :: CardDef
devoteeOfTheKey =
  (enemy "02294" "Devotee of the Key" WhereDoomAwaits 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Sorcerer]
    }

sethBishop :: CardDef
sethBishop =
  unique
    $ (enemy "02293" ("Seth Bishop" <:> "Sorcerer of Dunwich") WhereDoomAwaits 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 5
      , cdEvade = evade 5
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }
