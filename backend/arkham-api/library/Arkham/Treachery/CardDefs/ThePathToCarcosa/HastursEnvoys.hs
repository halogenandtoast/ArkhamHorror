module Arkham.Treachery.CardDefs.ThePathToCarcosa.HastursEnvoys where

import Arkham.Treachery.CardDefs.Import

theSignOfHastur :: CardDef
theSignOfHastur =
  peril
    (treachery "52070" "The Sign of Hastur" HastursEnvoys 2)
      { cdCardTraits = setFromList [Pact, Power]
      }
