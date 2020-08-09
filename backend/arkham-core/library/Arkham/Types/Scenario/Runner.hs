module Arkham.Types.Scenario.Runner where

import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Query
import Arkham.Types.Trait

type ScenarioRunner env
  = ( HasCount EnemyCount (InvestigatorLocation, [Trait]) env
    , HasQueue env
    , HasSet InScenarioInvestigatorId () env
    )

