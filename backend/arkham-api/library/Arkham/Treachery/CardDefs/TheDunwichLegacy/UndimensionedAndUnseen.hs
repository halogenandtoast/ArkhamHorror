module Arkham.Treachery.CardDefs.TheDunwichLegacy.UndimensionedAndUnseen where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

attractingAttention :: CardDef
attractingAttention =
  (treachery "02258" "Attracting Attention" UndimensionedAndUnseen 2)
    { cdKeywords = singleton Keyword.Surge
    }

ruinAndDestruction :: CardDef
ruinAndDestruction =
  (treachery "02257" "Ruin and Destruction" UndimensionedAndUnseen 3)
    { cdCardTraits = singleton Hazard
    }

theCreaturesTracks :: CardDef
theCreaturesTracks =
  (treachery "02259" "The Creatures' Tracks" UndimensionedAndUnseen 2)
    { cdCardTraits = singleton Terror
    , cdKeywords = singleton Keyword.Peril
    }

toweringBeasts :: CardDef
toweringBeasts =
  (treachery "02256" "Towering Beasts" UndimensionedAndUnseen 4)
    { cdKeywords = singleton Keyword.Peril
    }
