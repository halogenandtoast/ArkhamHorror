module Arkham.Treachery.CardDefs.TheCircleUndone.CityOfSins where

import Arkham.Treachery.CardDefs.Import

centuriesOfSecrets :: CardDef
centuriesOfSecrets =
  (treachery "05099" "Centuries of Secrets" CityOfSins 3)
    { cdCardTraits = singleton Curse
    }

evilPast :: CardDef
evilPast =
  (treachery "05098" "Evil Past" CityOfSins 2)
    { cdCardTraits = singleton Curse
    }
