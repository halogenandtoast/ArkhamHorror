module Arkham.Treachery.CardDefs.TheCircleUndone.Witchcraft where

import Arkham.Treachery.CardDefs.Import

bedeviled :: CardDef
bedeviled =
  (treachery "05094" "Bedeviled" Witchcraft 2)
    { cdCardTraits = singleton Hex
    }

diabolicVoices :: CardDef
diabolicVoices =
  (treachery "05092" "Diabolic Voices" Witchcraft 3)
    { cdCardTraits = singleton Curse
    }

wracked :: CardDef
wracked =
  (treachery "05093" "Wracked" Witchcraft 2)
    { cdCardTraits = singleton Hex
    }
