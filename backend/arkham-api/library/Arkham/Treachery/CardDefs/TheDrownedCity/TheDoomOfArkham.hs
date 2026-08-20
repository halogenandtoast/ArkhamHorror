module Arkham.Treachery.CardDefs.TheDrownedCity.TheDoomOfArkham where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

arkhamUnderAssault :: CardDef
arkhamUnderAssault =
  (treachery "11720" "Arkham Under Assault" TheDoomOfArkhamPartII 2)
    { cdCardTraits = setFromList [Scheme]
    }

eyesOfYchlecht :: CardDef
eyesOfYchlecht =
  (treachery "11717" "Eyes of Y'ch'lecht" TheDoomOfArkhamPartII 2)
    { cdCardTraits = setFromList [Power]
    , cdKeywords = setFromList [Keyword.Peril]
    }

grossPlasticity :: CardDef
grossPlasticity =
  (treachery "11719" "Gross Plasticity" TheDoomOfArkhamPartII 2)
    { cdCardTraits = setFromList [Power]
    , cdKeywords = setFromList [Keyword.Peril]
    }

layWaste :: CardDef
layWaste =
  (treachery "11716" "Lay Waste" TheDoomOfArkhamPartII 2) {cdCardTraits = setFromList [Hazard]}
