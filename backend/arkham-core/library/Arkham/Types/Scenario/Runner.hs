module Arkham.Types.Scenario.Runner where

import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Query
import Arkham.Types.Trait

type ScenarioRunner env
  = ( HasCount EnemyCount (InvestigatorLocation, [Trait]) env
    , HasQueue env
    , HasLog env
    , HasSet InScenarioInvestigatorId () env
    , HasId LeadInvestigatorId () env
    , HasSet InvestigatorId () env
    )

