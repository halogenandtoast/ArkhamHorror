module Arkham.Enemy.CardDefs.TheForgottenAge.Serpents where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

boaConstrictor :: CardDef
boaConstrictor =
  (enemy "04079" "Boa Constrictor" Serpents 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Creature, Serpent]
    , cdKeywords = singleton Keyword.Hunter
    , cdVengeancePoints = Just 2
    }

pitViper :: CardDef
pitViper =
  (enemy "04078" "Pit Viper" Serpents 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 1
    , cdCardTraits = setFromList [Creature, Serpent]
    , cdVengeancePoints = Just 1
    }
