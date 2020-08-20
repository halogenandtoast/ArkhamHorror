module Arkham.Types.Scenario.Runner where

import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.Query
import Arkham.Types.Trait

type ScenarioRunner env
  = ( HasCount EnemyCount (InvestigatorLocation, [Trait]) env
    , HasQueue env
    , HasSet InScenarioInvestigatorId () env
    , HasId LeadInvestigatorId () env
    , HasSet InvestigatorId () env
    , HasRecord env
    , HasCount PlayerCount () env
    , HasCount DoomCount EnemyId env
    , HasCount DoomCount () env
    , HasCount XPCount () env
    , HasSet EnemyId Trait env
    , HasSet ClosestEnemyId (InvestigatorId, [Trait]) env
    , HasSet VictoryDisplayCardCode () env
    )

