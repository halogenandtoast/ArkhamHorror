module Arkham.Treachery.CardDefs.ChildrenOfBlood.BloodMoon where

import Arkham.Treachery.CardDefs.Import

bloodMoon :: CardDef
bloodMoon =
  peril
    $ (treachery "13101" "Blood Moon" BloodMoon 3)
      { cdCardTraits = singleton Omen
      }
