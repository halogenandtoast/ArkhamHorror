module Arkham.Scenarios.UnionAndDisillusion.Helpers where

import Arkham.Action
import Arkham.Criteria
import Arkham.Field
import Arkham.Id
import Arkham.Location.Brazier
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Message

lightBrazier :: LocationId -> Message
lightBrazier locationId = UpdateLocation locationId (LocationBrazier ?=. Lit)

unlightBrazier :: LocationId -> Message
unlightBrazier locationId = UpdateLocation locationId (LocationBrazier ?=. Unlit)

pattern DuringCircleAction :: Criterion
pattern DuringCircleAction <- DuringSkillTest (SkillTestForAction (ActionIs Circle))
  where
    DuringCircleAction = DuringSkillTest (SkillTestForAction (ActionIs Circle))
