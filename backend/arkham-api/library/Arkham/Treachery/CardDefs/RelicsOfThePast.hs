module Arkham.Treachery.CardDefs.RelicsOfThePast where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

vengeantPast :: CardDef
vengeantPast =
  (treachery "90077" "Vengeant Past" RelicsOfThePast 1)
    { cdCardTraits = singleton Power
    , cdKeywords = singleton Keyword.Peril
    }
