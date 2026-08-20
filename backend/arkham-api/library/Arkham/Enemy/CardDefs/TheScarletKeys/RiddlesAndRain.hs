module Arkham.Enemy.CardDefs.TheScarletKeys.RiddlesAndRain where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

theRedGlovedManShroudedInMystery :: CardDef
theRedGlovedManShroudedInMystery =
  (enemy "09518" ("The Red-Gloved Man" <:> "Shrouded in Mystery") RiddlesAndRain 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 5
    , cdHealth = healthPerInvestigator 2
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Concealed TheRedGlovedMan (PerPlayer 1), Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }
