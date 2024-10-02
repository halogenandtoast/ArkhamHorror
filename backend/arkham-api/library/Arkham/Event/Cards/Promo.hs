module Arkham.Event.Cards.Promo where

import Arkham.Event.Cards.Import
import Arkham.Keyword qualified as Keyword

mysteriesRemain :: CardDef
mysteriesRemain =
  signature "01001"
    $ (event "98005" "Mysteries Remain" 0 Neutral)
      { cdSkills = [#combat, #intellect, #wild]
      , cdCardTraits = singleton Insight
      , cdKeywords = setFromList [Keyword.Replacement]
      , cdFastWindow = Just $ DuringTurn You
      , cdCriteria = Just $ youExist (at_ Anywhere)
      }
