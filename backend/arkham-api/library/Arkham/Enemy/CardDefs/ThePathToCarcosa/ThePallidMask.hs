module Arkham.Enemy.CardDefs.ThePathToCarcosa.ThePallidMask where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

catacombsDocent :: CardDef
catacombsDocent =
  (enemy "03258" "Catacombs Docent" ThePallidMask 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Lunatic]
    }

corpseDweller :: CardDef
corpseDweller =
  (enemy "03259" "Corpse Dweller" ThePallidMask 3)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 5
    , cdCardTraits = singleton Monster
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

specterOfDeath :: CardDef
specterOfDeath =
  doubleSided "03241"
    $ (enemy "03241b" ("Specter of Death" <:> "A Force From Beyond") ThePallidMask 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Monster, Geist, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }
