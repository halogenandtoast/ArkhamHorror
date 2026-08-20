module Arkham.Treachery.CardDefs.ReturnToTheCircleUndone.ReturnToUnionAndDisillusion where

import Arkham.Treachery.CardDefs.Import

brazierEnchantment :: CardDef
brazierEnchantment =
  (treachery "54048" "Brazier Enchantment" ReturnToUnionAndDisillusion 2)
    { cdCardTraits = setFromList [Curse, Hex]
    }
