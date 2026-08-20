module Arkham.Treachery.CardDefs.ReturnToTheDunwichLegacy.ResurgentEvils where

import Arkham.Treachery.CardDefs.Import

resurgentEvils :: CardDef
resurgentEvils =
  peril
    $ (treachery "51064" "Resurgent Evils" ResurgentEvils 3)
      { cdCardTraits = setFromList [Omen]
      }
