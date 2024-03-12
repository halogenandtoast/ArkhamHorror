module Arkham.Scenarios.DarkSideOfTheMoon.Helpers where

import Arkham.Classes.HasGame
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Token

getAlarmLevel :: HasGame m => InvestigatorId -> m Int
getAlarmLevel = fieldMap InvestigatorTokens (countTokens AlarmLevel)
