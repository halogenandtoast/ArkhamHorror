module Arkham.Enemy.CardDefs.TheDrownedCity.TheDrownedQuarter where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

medusa :: CardDef
medusa =
  (enemy "11551" "Medusa" TheDrownedQuarter 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Patrol (not_ FullyFloodedLocation)]
    }

seafloorLeviathan :: CardDef
seafloorLeviathan =
  doubleSided "11537"
    $ (enemy "11537b" ("Seafloor Leviathan" <:> "Giant Aquatic Medusoid") TheDrownedQuarter 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Massive, Keyword.Patrol (not_ FullyFloodedLocation)]
      , cdVictoryPoints = Just 2
      }

underseaParasite :: CardDef
underseaParasite =
  doubleSided "11549b"
    $ (enemy "11549" "Undersea Parasite" TheDrownedQuarter 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 5
      , cdEvade = evade 5
      , cdHealth = health 1
      , cdCardTraits = setFromList [Monster, Glyph, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
      }
