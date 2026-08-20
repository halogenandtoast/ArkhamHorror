module Arkham.Treachery.CardDefs.TheDrownedCity.ObsidianCanyons where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

acrophobia :: CardDef
acrophobia =
  (treachery "11666" "Acrophobia" ObsidianCanyons 2) {cdCardTraits = setFromList [Terror]}

erodedFrieze :: CardDef
erodedFrieze =
  (treachery "11664" "Eroded Frieze" ObsidianCanyons 1)
    { cdCardTraits = setFromList [Evidence, Glyph]
    , cdOtherSide = Just "11664b"
    , cdDoubleSided = True
    , cdRevelation = CannotBeCanceledRevelation
    }

lostInTheClouds :: CardDef
lostInTheClouds =
  (treachery "11669" "Lost in the Clouds" ObsidianCanyons 1)
    { cdCardTraits = setFromList [Blunder]
    , cdKeywords = setFromList [Keyword.Peril]
    }

stElmosFire :: CardDef
stElmosFire =
  (treachery "11665" "St. Elmo's Fire" ObsidianCanyons 2) {cdCardTraits = setFromList [Hazard]}

wingsOfTerror :: CardDef
wingsOfTerror =
  (treachery "11667" "Wings of Terror" ObsidianCanyons 2)
    { cdCardTraits = setFromList [Scheme, Terror]
    }
