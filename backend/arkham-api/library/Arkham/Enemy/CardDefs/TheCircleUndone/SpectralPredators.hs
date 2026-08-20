module Arkham.Enemy.CardDefs.TheCircleUndone.SpectralPredators where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

netherMist :: CardDef
netherMist =
  (enemy "05100" "Nether Mist" SpectralPredators 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Spectral]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

shadowHound :: CardDef
shadowHound =
  (enemy "05101" "Shadow Hound" SpectralPredators 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Spectral]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }
