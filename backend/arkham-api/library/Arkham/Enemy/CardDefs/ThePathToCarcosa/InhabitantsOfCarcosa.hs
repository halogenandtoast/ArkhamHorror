module Arkham.Enemy.CardDefs.ThePathToCarcosa.InhabitantsOfCarcosa where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

beastOfAldebaran :: CardDef
beastOfAldebaran =
  (enemy "03088" "Beast of Aldebaran" InhabitantsOfCarcosa 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 5
    , cdHealth = health 7
    , cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

spawnOfHali :: CardDef
spawnOfHali =
  (enemy "03089" "Spawn of Hali" InhabitantsOfCarcosa 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Retaliate
    }
