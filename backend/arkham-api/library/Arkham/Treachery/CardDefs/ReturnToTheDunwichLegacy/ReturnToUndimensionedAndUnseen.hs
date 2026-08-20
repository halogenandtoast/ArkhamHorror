module Arkham.Treachery.CardDefs.ReturnToTheDunwichLegacy.ReturnToUndimensionedAndUnseen where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

imperceptableCreature :: CardDef
imperceptableCreature =
  (treachery "51046" "Imperceptable Creature" ReturnToUndimensionedAndUnseen 2)
    { cdCardTraits = setFromList [Power]
    , cdKeywords = setFromList [Keyword.Surge]
    }
