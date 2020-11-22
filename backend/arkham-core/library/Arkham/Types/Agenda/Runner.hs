module Arkham.Types.Agenda.Runner where

import Arkham.Import

import Arkham.Types.Trait

type AgendaRunner env
  = ( HasId LeadInvestigatorId env ()
    , HasCount ClueCount env InvestigatorId
    , HasCount ClueCount env LocationId
    , HasCount DiscardCount env InvestigatorId
    , HasCount DoomCount env ()
    , HasCount EnemyCount env (LocationId, [Trait])
    , HasCount PlayerCount env ()
    , HasId (Maybe StoryEnemyId) env CardCode
    , HasId (Maybe LocationId) env LocationName
    , HasId LocationId env EnemyId
    , HasQueue env
    , HasList LocationName env ()
    , HasSet ActId env ()
    , HasSet ClosestLocationId env (LocationId, LocationId)
    , HasSet CompletedScenarioId env ()
    , HasSet EnemyId env ()
    , HasSet EnemyId env LocationId
    , HasSet EnemyId env Trait
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env EnemyId
    , HasSet LocationId env ()
    , HasSet LocationId env [Trait]
    , HasSet UnengagedEnemyId env ()
    )

