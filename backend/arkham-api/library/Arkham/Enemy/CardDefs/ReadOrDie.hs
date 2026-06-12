module Arkham.Enemy.CardDefs.ReadOrDie where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

namerOfTheDead :: CardDef
namerOfTheDead =
  (enemy "90007" ("Namer of the Dead" <:> "Presence Within the Grimoire") ReadOrDie 1)
    { cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdKeywords = singleton Keyword.Hunter
    }
