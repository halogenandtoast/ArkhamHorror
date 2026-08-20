module Arkham.Treachery.CardDefs.NightOfTheZealot.AncientEvils where

import Arkham.Treachery.CardDefs.Import

ancientEvils :: CardDef
ancientEvils =
  (treachery "01166" "Ancient Evils" AncientEvils 3)
    { cdCardTraits = setFromList [Omen]
    }
