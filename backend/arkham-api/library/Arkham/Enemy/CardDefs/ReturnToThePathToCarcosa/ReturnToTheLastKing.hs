module Arkham.Enemy.CardDefs.ReturnToThePathToCarcosa.ReturnToTheLastKing where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

crazedGuest :: CardDef
crazedGuest =
  doubleSided "52024"
    $ (enemy "52024b" "Crazed Guest" ReturnToTheLastKing 2)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Monster, Lunatic]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

dianneDevineKnowsWhatYoureUpTo :: CardDef
dianneDevineKnowsWhatYoureUpTo =
  unique
    $ doubleSided "52023"
    $ (enemy "52023b" ("Dianne Devine" <:> "Knows What You're Up To") ReturnToTheLastKing 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Monster, Cultist, Lunatic, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 1
      }
