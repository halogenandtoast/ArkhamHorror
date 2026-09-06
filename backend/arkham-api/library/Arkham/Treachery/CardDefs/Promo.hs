module Arkham.Treachery.CardDefs.Promo where

import Arkham.Keyword qualified as Keyword
import Arkham.Trait qualified as Trait
import Arkham.Treachery.CardDefs.Import

theDirgeOfReason :: CardDef
theDirgeOfReason =
  signature "01001"
    $ (weakness "98006" "The Dirge of Reason")
      { cdCardTraits = setFromList [Madness]
      , cdKeywords = setFromList [Keyword.Replacement]
      }

toFightTheBlackWind :: CardDef
toFightTheBlackWind =
  signature "05001"
    $ (weakness "98012" "To Fight the Black Wind")
      { cdCardTraits = setFromList [Task, Trait.Dreamlands]
      , cdKeywords = setFromList [Keyword.Replacement]
      }

yaztaroth :: CardDef
yaztaroth =
  signature "07004"
    $ (weakness "98018" "Yaztaroth")
      { cdCardTraits = setFromList [Curse, Pact]
      , cdUnique = True
      , cdKeywords = setFromList [Keyword.Replacement]
      }

liberOmniumFinium :: CardDef
liberOmniumFinium =
  signature "11014"
    $ (weakness "98021" "Liber Omnium Finium")
      { cdCardTraits = setFromList [Endtimes]
      , cdKeywords = setFromList [Keyword.Replacement]
      }
