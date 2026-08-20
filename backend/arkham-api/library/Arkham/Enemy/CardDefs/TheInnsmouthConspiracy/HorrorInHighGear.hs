module Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.HorrorInHighGear where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

hitVan :: CardDef
hitVan =
  (enemy "07214" "Hit Van" HorrorInHighGear 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Vehicle, Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

hybridAssassin :: CardDef
hybridAssassin =
  (enemy "07215" "Hybrid Assassin" HorrorInHighGear 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Vehicle, Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

pursuingMotorcar :: CardDef
pursuingMotorcar =
  (enemy "07213" "Pursuing Motorcar" HorrorInHighGear 2)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Vehicle, Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

theTerrorOfDevilReefRelentlessMonstrosity :: CardDef
theTerrorOfDevilReefRelentlessMonstrosity =
  doubleSided "07199"
    $ (enemy "07199b" ("The Terror of Devil Reef" <:> "Relentless Monstrosity") HorrorInHighGear 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 6
      , cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      }
