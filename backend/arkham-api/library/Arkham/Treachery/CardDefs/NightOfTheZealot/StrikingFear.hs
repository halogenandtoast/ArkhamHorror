module Arkham.Treachery.CardDefs.NightOfTheZealot.StrikingFear where

import Arkham.Treachery.CardDefs.Import

dissonantVoices :: CardDef
dissonantVoices =
  (treachery "01165" "Dissonant Voices" StrikingFear 2)
    { cdCardTraits = setFromList [Terror]
    }

frozenInFear :: CardDef
frozenInFear =
  (treachery "01164" "Frozen in Fear" StrikingFear 2)
    { cdCardTraits = setFromList [Terror]
    }

rottingRemains :: CardDef
rottingRemains =
  (treachery "01163" "Rotting Remains" StrikingFear 3)
    { cdCardTraits = setFromList [Terror]
    }
