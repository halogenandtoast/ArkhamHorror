module Arkham.Treachery.CardDefs.TheScarletKeys.SecretWar where

import Arkham.Treachery.CardDefs.Import

memoryVariant :: CardDef
memoryVariant =
  (treachery "09735" "Memory Variant" SecretWar 2)
    { cdCardTraits = setFromList [Power]
    }

secretsLost :: CardDef
secretsLost =
  (treachery "09736" "Secrets Lost" SecretWar 3)
    { cdCardTraits = setFromList [Power]
    }
