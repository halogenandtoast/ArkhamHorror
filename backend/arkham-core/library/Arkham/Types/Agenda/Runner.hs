module Arkham.Types.Agenda.Runner where

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Trait

type AgendaRunner env
  = ( HasQueue env
    , HasCount ClueCount env InvestigatorId
    , HasCount ClueCount env LocationId
    , HasCount DiscardCount env InvestigatorId
    , HasCount DoomCount env ()
    , HasCount EnemyCount env (LocationId, [Trait])
    , HasCount EnemyCount env (LocationMatcher, [Trait])
    , HasCount PlayerCount env ()
    , HasCount ScenarioDeckCount env ()
    , HasId (Maybe LocationId) env LocationMatcher
    , HasId (Maybe LocationId) env (Direction, LocationId)
    , HasId (Maybe StoryEnemyId) env CardCode
    , HasId (Maybe StoryTreacheryId) env CardCode
    , HasId CardCode env EnemyId
    , HasId LeadInvestigatorId env ()
    , HasId LocationId env EnemyId
    , HasId LocationId env InvestigatorId
    , HasList LocationName env ()
    , HasSet ActId env ()
    , HasSet ClosestPathLocationId env (LocationId, LocationId)
    , HasSet ClosestPathLocationId env (LocationId, LocationMatcher)
    , HasSet CompletedScenarioId env ()
    , HasSet EnemyId env ()
    , HasSet EnemyId env ([Trait], LocationId)
    , HasSet EnemyId env LocationId
    , HasSet EnemyId env LocationMatcher
    , HasSet EnemyId env Trait
    , HasSet InScenarioInvestigatorId env ()
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env EnemyId
    , HasSet InvestigatorId env LocationMatcher
    , HasSet InvestigatorId env LocationId
    , HasSet LocationId env ()
    , HasSet LocationId env [Trait]
    , HasSet Trait env EnemyId
    , HasSet UnengagedEnemyId env ()
    )

