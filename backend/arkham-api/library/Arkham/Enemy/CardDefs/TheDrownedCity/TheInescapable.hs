module Arkham.Enemy.CardDefs.TheDrownedCity.TheInescapable where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

theInescapable :: CardDef
theInescapable =
  unique
    $ (enemy "11744" ("The Inescapable" <:> "Tireless Pursuer") TheInescapable 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 6
      , cdCardTraits = setFromList [Monster, StarSpawn, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Relentless]
      , cdVictoryPoints = Just 0
      }
