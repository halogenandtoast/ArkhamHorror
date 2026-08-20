module Arkham.Enemy.CardDefs.ThePathToCarcosa.CurtainCall where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

royalEmissary :: CardDef
royalEmissary =
  unique
    $ (enemy "03060" ("Royal Emissary" <:> "Messenger from Aldebaran") CurtainCall 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords =
          setFromList
            [Keyword.Massive, Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

theManInThePallidMask :: CardDef
theManInThePallidMask =
  unique
    $ (weakness "03059" "The Man in the Pallid Mask")
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Elite]
      , cdKeywords = setFromList [Keyword.Aloof]
      , cdEncounterSet = Just CurtainCall
      , cdEncounterSetQuantity = Just 1
      }
