module Arkham.Enemy.CardDefs.NightOfTheZealot.ReturnCultOfUmordhoth where

import Arkham.Enemy.CardDefs.Import

almaHill :: CardDef
almaHill =
  unique
    $ ( enemy
          "50046"
          ("Alma Hill" <:> "The Inquisitive Historian")
          ReturnCultOfUmordhoth
          1
      )
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

billyCooper :: CardDef
billyCooper =
  unique
    $ (enemy "50045" ("Billy Cooper" <:> "The Crooked Cop") ReturnCultOfUmordhoth 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 5
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

jeremiahPierce :: CardDef
jeremiahPierce =
  unique
    $ ( enemy
          "50044"
          ("Jeremiah Pierce" <:> "Your Next-Door Neighbor")
          ReturnCultOfUmordhoth
          1
      )
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }
