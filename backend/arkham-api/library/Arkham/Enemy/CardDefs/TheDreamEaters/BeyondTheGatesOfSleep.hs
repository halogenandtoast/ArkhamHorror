module Arkham.Enemy.CardDefs.TheDreamEaters.BeyondTheGatesOfSleep where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

ancientZoog :: CardDef
ancientZoog =
  (enemy "06061" "Ancient Zoog" BeyondTheGatesOfSleep 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Zoog, Elite]
    , cdKeywords = singleton Keyword.Aloof
    }

kamanThah :: CardDef
kamanThah =
  doubleSided "06057b"
    $ (enemy "06057" ("Kaman-Thah" <:> "Priest of the Dreamlands") BeyondTheGatesOfSleep 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Dreamlands, Warden, Elite]
      , cdUnique = True
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
      }

laboringGug :: CardDef
laboringGug =
  (enemy "06060" "Laboring Gug" BeyondTheGatesOfSleep 1)
    { cdHealthDamage = healthDamage 3
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Gug]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }

nasht :: CardDef
nasht =
  doubleSided "06058b"
    $ (enemy "06058" ("Nasht" <:> "Priest of the Dreamlands") BeyondTheGatesOfSleep 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Dreamlands, Warden, Elite]
      , cdUnique = True
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
      }
