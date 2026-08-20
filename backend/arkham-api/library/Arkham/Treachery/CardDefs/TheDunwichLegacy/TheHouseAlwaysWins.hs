module Arkham.Treachery.CardDefs.TheDunwichLegacy.TheHouseAlwaysWins where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

arousingSuspicions :: CardDef
arousingSuspicions =
  treachery "02082" "Arousing Suspicions" TheHouseAlwaysWins 2

somethingInTheDrinks :: CardDef
somethingInTheDrinks =
  (treachery "02081" "Something in the Drinks" TheHouseAlwaysWins 2)
    { cdCardTraits = setFromList [Poison, Illicit]
    , cdKeywords = setFromList [Keyword.Surge]
    }
