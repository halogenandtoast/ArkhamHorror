module Arkham.Enemy.CardDefs.NightOfTheZealot.CultOfUmordhoth where

import Arkham.Enemy.CardDefs.Import

hermanCollins :: CardDef
hermanCollins =
  unique
    $ (enemy "01138" ("Herman Collins" <:> "The Undertaker") CultOfUmordhoth 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

peterWarren :: CardDef
peterWarren =
  unique
    $ (enemy "01139" ("Peter Warren" <:> "The Occult Professor") CultOfUmordhoth 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

ruthTurner :: CardDef
ruthTurner =
  unique
    $ (enemy "01141" ("Ruth Turner" <:> "The Mortician") CultOfUmordhoth 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 2
      , cdEvade = evade 5
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

victoriaDevereux :: CardDef
victoriaDevereux =
  unique
    $ (enemy "01140" ("Victoria Devereux" <:> "The Collector") CultOfUmordhoth 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

wolfManDrew :: CardDef
wolfManDrew =
  unique
    $ (enemy "01137" ("\"Wolf-Man\" Drew" <:> "The Cannibal") CultOfUmordhoth 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }
