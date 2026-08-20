module Arkham.Treachery.CardDefs.NightOfTheZealot.Ghouls where

import Arkham.Treachery.CardDefs.Import

graspingHands :: CardDef
graspingHands =
  (treachery "01162" "Grasping Hands" Ghouls 3)
    { cdCardTraits = setFromList [Hazard]
    }
