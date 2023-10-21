module Arkham.Act.Helpers (
  module X,
  module Arkham.Act.Helpers,
) where

import Arkham.Act.Types
import Arkham.Classes.Entity
import Arkham.Game.Helpers as X
import Arkham.Message
import Arkham.Source

advancedWithOther :: ActAttrs -> Message
advancedWithOther attrs = AdvanceAct (toId attrs) (toSource attrs) #other
