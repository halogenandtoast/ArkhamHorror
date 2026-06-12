module Arkham.Enemy.CardDefs.EnthrallingEncore where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

sinisterSoloist :: CardDef
sinisterSoloist =
  unique
    $ (enemy "90097" ("Sinister Soloist" <:> "Minstrel of Carcosa") EnthrallingEncore 1)
      { cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Aloof, Keyword.Elusive, Keyword.Hunter]
      , cdVictoryPoints = Just 2
      }
