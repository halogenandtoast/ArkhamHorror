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
    , HasSet AgendaId () env
    , HasSet ClosestEnemyId (InvestigatorId, [Trait]) env
    , HasSet ClosestEnemyId InvestigatorId env
    , HasSet ConnectedLocationId LocationId env
    , HasSet EnemyId LocationId env
    , HasSet EnemyId Trait env
    , HasSet InScenarioInvestigatorId () env
    , HasSet InvestigatorId () env
    , HasSet LocationId [Trait] env
    , HasSet Trait LocationId env
    , HasSet VictoryDisplayCardCode () env
    , HasSource ForSkillTest env
    )

