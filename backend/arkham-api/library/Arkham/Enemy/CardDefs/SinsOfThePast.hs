module Arkham.Enemy.CardDefs.SinsOfThePast where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

vengefulSpecter :: CardDef
vengefulSpecter =
  (enemy "84041" ("Vengeful Specter" <:> "The First Victim") SinsOfThePast 1)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 5
    , cdHealth = healthPerInvestigator 4
    , cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdKeywords = setFromList [Keyword.Patrol "Room 245", Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }
