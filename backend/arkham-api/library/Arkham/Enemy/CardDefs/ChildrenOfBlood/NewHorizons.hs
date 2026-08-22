module Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

zburamoarteLethargicBeast :: CardDef
zburamoarteLethargicBeast =
  (enemy "13058" ("Zburamoarte" <:> "Lethargic Beast") NewHorizons 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdHealth = health 8
    , cdEvade = evade 4
    , cdCardTraits = setFromList [Monster, Abomination, Elite]
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Predator, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

zburamoarteTheSourceOfTheBlight :: CardDef
zburamoarteTheSourceOfTheBlight =
  (enemy "13059" ("Zburamoarte" <:> "The Source of the Blight") NewHorizons 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdHealth = health 8
    , cdEvade = evade 4
    , cdCardTraits = setFromList [Monster, Abomination, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Massive, Keyword.Predator, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

zburamoarteProgenitorOfMonsters :: CardDef
zburamoarteProgenitorOfMonsters =
  (enemy "13060" ("Zburamoarte" <:> "Progenitor of Monsters") NewHorizons 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 5
    , cdHealth = health 8
    , cdEvade = evade 4
    , cdCardTraits = setFromList [Monster, Abomination, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Massive, Keyword.Predator, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

javierRivera :: CardDef
javierRivera =
  (enemy "13061" ("Javier Rivera" <:> "New Horizons Manager") NewHorizons 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 4
    , cdHealth = health 3
    , cdEvade = evade 4
    , cdCardTraits = setFromList [Humanoid, Civilian, Elite]
    , cdKeywords = setFromList [Keyword.Doomed, Keyword.Hunter]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

nightWatchman :: CardDef
nightWatchman =
  (enemy "13062" ("Night Watchman" <:> "Bloodthirsty Butcher") NewHorizons 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdHealth = health 6
    , cdEvade = evade 3
    , cdCardTraits = setFromList [Humanoid, Monster, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Predator, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

factoryWorker :: CardDef
factoryWorker =
  (enemy "13063" "Factory Worker" NewHorizons 4)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdHealth = health 1
    , cdEvade = evade 1
    , cdCardTraits = setFromList [Humanoid, Civilian]
    , cdKeywords =
        setFromList [Keyword.Doomed, Keyword.Patrol (not_ $ LocationWithEnemy $ EnemyWithTrait Elite)]
    }

blightedWorker :: CardDef
blightedWorker =
  (enemy "13064" "Blighted Worker" NewHorizons 4)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdHealth = health 1
    , cdEvade = evade 2
    , cdCardTraits = setFromList [Humanoid, Monster]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
