module Arkham.Types.Agenda.Runner where

import Arkham.Import

import Arkham.Types.LocationMatcher
import Arkham.Types.Trait

type AgendaRunner env
  = ( HasId LeadInvestigatorId env ()
    , HasCount ClueCount env InvestigatorId
    , HasCount ClueCount env LocationId
    , HasCount DiscardCount env InvestigatorId
    , HasCount DoomCount env ()
    , HasCount EnemyCount env (LocationId, [Trait])
    , HasCount EnemyCount env (LocationMatcher, [Trait])
    , HasCount PlayerCount env ()
    , HasId (Maybe StoryEnemyId) env CardCode
    , HasId (Maybe LocationId) env LocationName
    , HasId LocationId env EnemyId
    , HasQueue env
    , HasList LocationName env ()
    , HasSet ActId env ()
    , HasSet ClosestPathLocationId env (LocationId, LocationId)
    , HasSet ClosestPathLocationId env (LocationId, LocationMatcher)
    , HasSet CompletedScenarioId env ()
    , HasSet EnemyId env ()
    , HasSet EnemyId env LocationId
    , HasSet EnemyId env Trait
    , HasSet EnemyId env ([Trait], LocationId)
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env EnemyId
    , HasSet InvestigatorId env LocationName
    , HasSet LocationId env ()
    , HasSet LocationId env [Trait]
    , HasSet UnengagedEnemyId env ()
    , HasSet EnemyId env LocationMatcher
    , HasSet Trait env EnemyId
    )

