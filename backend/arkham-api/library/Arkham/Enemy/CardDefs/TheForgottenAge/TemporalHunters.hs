module Arkham.Enemy.CardDefs.TheForgottenAge.TemporalHunters where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

tindalosAlpha :: CardDef
tindalosAlpha =
  (enemy "53077" "Tindalos Alpha" TemporalHunters 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Extradimensional, Tindalos]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
    }
