module Arkham.Enemy.CardDefs.TheDrownedCity.TheWesternWall where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

deepOneMatron :: CardDef
deepOneMatron =
  (enemy "11533" "Deep One Matron" TheWesternWall 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 6
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

huntingParasite :: CardDef
huntingParasite =
  unique
    $ (weakness "11535" "Hunting Parasite")
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 1
      , cdCardTraits = setFromList [Monster, Stowaway]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      , cdEncounterSet = Just TheWesternWall
      , cdEncounterSetQuantity = Just 1
      }
