module Arkham.Treachery.CardDefs.TheForgottenAge.VenomousHate where

import Arkham.Treachery.CardDefs.Import

wrathOfYig :: CardDef
wrathOfYig =
  (treachery "53080" "Wrath of Yig" VenomousHate 1)
    { cdCardTraits = setFromList [Power]
    }
