module Arkham.Enemy.CardDefs.Core2026 where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

blackChamberOperative :: CardDef
blackChamberOperative =
  (weakness "12009" "Black Chamber Operative")
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

theNamelessLurker :: CardDef
theNamelessLurker =
  unique
    $ (basicWeakness "12099" "The Nameless Lurker")
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 1
      , cdCardTraits = setFromList [Humanoid, Monster]
      , cdKeywords = setFromList [Keyword.Aloof]
      }
