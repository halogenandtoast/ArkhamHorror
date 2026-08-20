module Arkham.Treachery.CardDefs.NightOfTheZealot.AgentsOfYogSothoth where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

offerOfPower :: CardDef
offerOfPower =
  (treachery "01178" "Offer of Power" AgentsOfYogSothoth 2)
    { cdCardTraits = setFromList [Pact]
    , cdKeywords = setFromList [Keyword.Peril]
    }
