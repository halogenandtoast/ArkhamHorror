module Arkham.Enemy.CardDefs.TheCircleUndone.BloodthirstySpirits where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

screechingBanshee :: CardDef
screechingBanshee =
  (enemy "54074" "Screeching Banshee" BloodthirstySpirits 2)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Geist, Spectral]
    , cdKeywords = singleton Keyword.Hunter
    }
