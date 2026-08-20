module Arkham.Enemy.CardDefs.CarnevaleOfHorrors where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

balefulReveler :: CardDef
balefulReveler =
  unique
    $ doubleSided "82002"
    $ (enemy "82002b" ("Baleful Reveler" <:> "Spreading Chaos") CarnevaleOfHorrors 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

carnevaleSentinel :: CardDef
carnevaleSentinel =
  (enemy "82029" "Carnevale Sentinel" CarnevaleOfHorrors 3)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

cnidathqua :: CardDef
cnidathqua =
  unique
    $ (enemy "82027" ("Cnidathqua" <:> "The Many-armed Beast") CarnevaleOfHorrors 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdHealth = healthPerInvestigator 8
      , cdCardTraits = setFromList [Monster, AncientOne, Elite]
      }

donLagorio :: CardDef
donLagorio =
  unique
    $ doubleSided "82017b"
    $ (enemy "82017" ("Don Lagorio" <:> "Secret Servant") CarnevaleOfHorrors 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Servitor, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 1
      }

elisabettaMagro :: CardDef
elisabettaMagro =
  unique
    $ doubleSided "82018b"
    $ ( enemy
          "82018"
          ("Elisabetta Magro" <:> "High Servant of the Order")
          CarnevaleOfHorrors
          1
      )
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Lodge, Elite]
      , cdKeywords = setFromList [Keyword.Aloof]
      , cdVictoryPoints = Just 1
      }

poleman :: CardDef
poleman =
  (enemy "82028" "Poleman" CarnevaleOfHorrors 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

salvatoreNeri :: CardDef
salvatoreNeri =
  unique
    $ doubleSided "82019b"
    $ ( enemy
          "82019"
          ("Salvatore Neri" <:> "Master of Illusions")
          CarnevaleOfHorrors
          1
      )
      { cdSanityDamage = sanityDamage 2
      , cdFight = fightUnknown
      , cdEvade = evadeUnknown
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

savioCorvi :: CardDef
savioCorvi =
  unique
    $ doubleSided "82020b"
    $ (enemy "82020" ("Savio Corvi" <:> "Dark Lurker") CarnevaleOfHorrors 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 1
      }

writhingAppendage :: CardDef
writhingAppendage =
  (enemy "82030" "Writhing Appendage" CarnevaleOfHorrors 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Tentacle]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }
