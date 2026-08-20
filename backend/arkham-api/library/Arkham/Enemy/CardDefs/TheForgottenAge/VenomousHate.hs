module Arkham.Enemy.CardDefs.TheForgottenAge.VenomousHate where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

serpentGuardian :: CardDef
serpentGuardian =
  (enemy "53079" "Serpent Guardian" VenomousHate 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 4
    , cdEvade = evade 1
    , cdHealth = health 5
    , cdCardTraits = setFromList [Construct, Serpent]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

vengefulSerpent :: CardDef
vengefulSerpent =
  (enemy "53078" "Vengeful Serpent" VenomousHate 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVengeancePoints = Just 0
    }
