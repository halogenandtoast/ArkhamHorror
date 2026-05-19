module Arkham.Treachery.CardDefs.Promo where

import Arkham.Treachery.CardDefs.Import
import Arkham.Keyword qualified as Keyword
import Arkham.Trait qualified as Trait

theDirgeOfReason :: CardDef
theDirgeOfReason =
  (weakness "98006" "The Dirge of Reason")
    { cdCardTraits = setFromList [Madness]
    , cdKeywords = setFromList [Keyword.Replacement]
    }

toFightTheBlackWind :: CardDef
toFightTheBlackWind =
  (weakness "98012" "To Fight the Black Wind")
    { cdCardTraits = setFromList [Task, Trait.Dreamlands]
    , cdKeywords = setFromList [Keyword.Replacement]
    }

yaztaroth :: CardDef
yaztaroth =
  (weakness "98018" "Yaztaroth")
    { cdCardTraits = setFromList [Curse, Pact]
    , cdUnique = True
    , cdKeywords = setFromList [Keyword.Replacement]
    }

liberOmniumFinium :: CardDef
liberOmniumFinium =
  (weakness "98021" "Liber Omnium Finium")
    { cdCardTraits = setFromList [Endtimes]
    , cdKeywords = setFromList [Keyword.Replacement]
    }
