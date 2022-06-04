module Arkham.Helpers.Scenario where

import Arkham.Prelude

import Arkham.Classes.Query
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenario.Attrs

scenarioField
  :: (Monad m, Query ScenarioMatcher m, Projection m ScenarioAttrs)
  => Field ScenarioAttrs a
  -> m a
scenarioField fld = scenarioFieldMap fld id

scenarioFieldMap
  :: (Monad m, Query ScenarioMatcher m, Projection m ScenarioAttrs)
  => Field ScenarioAttrs a
  -> (a -> b)
  -> m b
scenarioFieldMap fld f = selectJust TheScenario >>= fieldMap fld f
