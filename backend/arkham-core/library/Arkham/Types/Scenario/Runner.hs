module Arkham.Types.Scenario.Runner where

import Arkham.Types.AgendaId
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait

type ScenarioRunner env
  = ( HasCount env DoomCount ()
    , HasCount env DoomCount EnemyId
    , HasCount env EnemyCount [Trait]
    , HasCount env PlayerCount ()
    , HasCount env XPCount ()
    , HasCount env EnemyCount (InvestigatorLocation, [Trait])
    , HasId CardCode EnemyId env
    , HasId LeadInvestigatorId () env
    , HasId LocationId InvestigatorId env
    , HasQueue env
    , HasRecord env
    , HasSet AgendaId env ()
    , HasSet ClosestEnemyId env (InvestigatorId, [Trait])
    , HasSet ClosestEnemyId env InvestigatorId
    , HasSet ConnectedLocationId env LocationId
    , HasSet EnemyId env LocationId
    , HasSet EnemyId env Trait
    , HasSet InScenarioInvestigatorId env ()
    , HasSet InvestigatorId env ()
    , HasSet LocationId env [Trait]
    , HasSet Trait env LocationId
    , HasSet VictoryDisplayCardCode env ()
    , HasSource ForSkillTest env
    )

