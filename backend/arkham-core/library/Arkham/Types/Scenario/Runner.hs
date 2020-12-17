module Arkham.Types.Scenario.Runner where

import Arkham.Prelude

import Arkham.Types.AgendaId
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.ScenarioId
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Target
import Arkham.Types.Trait

type ScenarioRunner env
  = ( HasCount DoomCount env ()
    , HasCount DoomCount env EnemyId
    , HasCount EnemyCount env [Trait]
    , HasCount PlayerCount env ()
    , HasCount ResourceCount env InvestigatorId
    , HasCount XPCount env ()
    , HasCount EnemyCount env (InvestigatorLocation, [Trait])
    , HasId CardCode env EnemyId
    , HasId LeadInvestigatorId env ()
    , HasId LocationId env InvestigatorId
    , HasId (Maybe StoryEnemyId) env CardCode
    , HasQueue env
    , HasRecord env
    , HasSet AgendaId env ()
    , HasSet ClosestEnemyId env (InvestigatorId, [Trait])
    , HasSet ClosestEnemyId env InvestigatorId
    , HasSet CompletedScenarioId env ()
    , HasSet ConnectedLocationId env LocationId
    , HasSet EnemyId env LocationId
    , HasSet EnemyId env Trait
    , HasSet InScenarioInvestigatorId env ()
    , HasSet InvestigatorId env ()
    , HasSet LocationId env [Trait]
    , HasSet ScenarioLogKey env ()
    , HasSet Trait env LocationId
    , HasSet VictoryDisplayCardCode env ()
    , HasSource ForSkillTest env
    )

