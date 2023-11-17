module Arkham.Act.Helpers (
  module X,
  module Arkham.Act.Helpers,
) where

import Arkham.Act.Types
import Arkham.Classes.Entity
import Arkham.Cost
import Arkham.Game.Helpers as X
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Source

advancedWithOther :: ActAttrs -> Message
advancedWithOther attrs = AdvanceAct (toId attrs) (toSource attrs) #other

groupClueCost :: GameValue -> Maybe Cost
groupClueCost val = Just (GroupClueCost val Anywhere)
