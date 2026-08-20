module Arkham.Treachery.CardDefs.ReturnToTheDunwichLegacy.ErraticFear where

import Arkham.Treachery.CardDefs.Import

idleHands :: CardDef
idleHands =
  (treachery "51069" "Idle Hands" ErraticFear 2)
    { cdCardTraits = setFromList [Terror]
    }

needForKnowledge :: CardDef
needForKnowledge =
  (treachery "51070" "Need for Knowledge" ErraticFear 2)
    { cdCardTraits = setFromList [Terror]
    }

violentCommands :: CardDef
violentCommands =
  (treachery "51068" "Violent Commands" ErraticFear 2)
    { cdCardTraits = setFromList [Terror]
    }
