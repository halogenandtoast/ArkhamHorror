module Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.TheLostSister where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

cavernMoss :: CardDef
cavernMoss =
  (enemy "10585" "Cavern Moss" TheLostSister 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Flora, Mutated]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }

crustaceanHybridInTheDark :: CardDef
crustaceanHybridInTheDark =
  doubleSided "10584a"
    $ (enemy "10584b" ("Crustacean Hybrid" <:> "In the Dark") TheLostSister 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [Creature, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

crustaceanHybridInTheLight :: CardDef
crustaceanHybridInTheLight =
  doubleSided "10584b"
    $ (enemy "10584a" ("Crustacean Hybrid" <:> "In the Light") TheLostSister 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 3
      , cdCardTraits = setFromList [Creature, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

limulusHybridInTheDark :: CardDef
limulusHybridInTheDark =
  doubleSided "10583a"
    $ (enemy "10583b" ("Limulus Hybrid" <:> "In the Dark") TheLostSister 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 5
      , cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

limulusHybridInTheLight :: CardDef
limulusHybridInTheLight =
  doubleSided "10583b"
    $ (enemy "10583a" ("Limulus Hybrid" <:> "In the Light") TheLostSister 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 5
      , cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Elusive, Keyword.Massive]
      , cdVictoryPoints = Just 2
      }
