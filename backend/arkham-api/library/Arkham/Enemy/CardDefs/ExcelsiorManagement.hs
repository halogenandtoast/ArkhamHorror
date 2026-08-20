module Arkham.Enemy.CardDefs.ExcelsiorManagement where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

hotelManager :: CardDef
hotelManager =
  (enemy "84032" ("Hotel Manager" <:> "Let the Feast Begin") ExcelsiorManagement 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = healthPerInvestigator 6
    , cdCardTraits = setFromList [Monster, Staff, Elite]
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

hotelSecurity :: CardDef
hotelSecurity =
  (enemy "84033" "Hotel Security" ExcelsiorManagement 3)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, Staff]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }
