module Arkham.Treachery.CardDefs.TheForgottenAge.ForgottenRuins where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

ancestralFear :: CardDef
ancestralFear =
  (treachery "04093" "Ancestral Fear" ForgottenRuins 2)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Surge]
    , cdVengeancePoints = Just 1
    }

deepDark :: CardDef
deepDark =
  (treachery "04094" "Deep Dark" ForgottenRuins 3)
    { cdCardTraits = singleton Hazard
    }

illOmen :: CardDef
illOmen =
  (treachery "04092" "Ill Omen" ForgottenRuins 2)
    { cdCardTraits = setFromList [Omen, Terror]
    , cdKeywords = singleton Keyword.Peril
    }
