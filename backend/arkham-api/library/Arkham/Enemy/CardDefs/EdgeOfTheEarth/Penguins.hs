module Arkham.Enemy.CardDefs.EdgeOfTheEarth.Penguins where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

giantAlbinoPenguin :: CardDef
giantAlbinoPenguin =
  (enemy "08708" "Giant Albino Penguin" Penguins 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Keyword.Aloof]
    }
