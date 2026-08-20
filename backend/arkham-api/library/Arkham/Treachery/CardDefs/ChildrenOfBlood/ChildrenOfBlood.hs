module Arkham.Treachery.CardDefs.ChildrenOfBlood.ChildrenOfBlood where

import Arkham.Treachery.CardDefs.Import

graspingHands :: CardDef
graspingHands =
  (treachery "13104" "Grasping Hands" ChildrenOfBlood 2)
    { cdCardTraits = singleton Hazard
    }
