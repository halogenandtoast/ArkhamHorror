module Arkham.Types.Agenda.Runner where

import Arkham.Import

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
    , HasId (Maybe LocationId) env LocationMatcher
    , HasId (Maybe StoryEnemyId) env CardCode
    , HasId (Maybe StoryTreacheryId) env CardCode
    , HasId CardCode env EnemyId
    , HasId LeadInvestigatorId env ()
    , HasId LocationId env EnemyId
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
    , HasSet LocationId env ()
    , HasSet LocationId env [Trait]
    , HasSet Trait env EnemyId
    , HasSet UnengagedEnemyId env ()
    )

