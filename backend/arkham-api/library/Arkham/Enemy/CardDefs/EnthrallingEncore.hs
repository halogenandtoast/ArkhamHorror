module Arkham.Enemy.CardDefs.EnthrallingEncore where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

sinisterSoloist :: CardDef
sinisterSoloist =
  unique
    $ (enemy "90097" ("Sinister Soloist" <:> "Minstrel of Carcosa") EnthrallingEncore 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 5
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Aloof, Keyword.Elusive, Keyword.Hunter]
      , cdVictoryPoints = Just 2
      }
