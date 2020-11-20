module Arkham.Types.Agenda.Runner where

import Arkham.Import

import Arkham.Types.Trait

type AgendaRunner env
  = ( HasId LeadInvestigatorId () env
    , HasCount env ClueCount InvestigatorId
    , HasCount env ClueCount LocationId
    , HasCount env DoomCount ()
    , HasCount env EnemyCount (LocationId, [Trait])
    , HasCount env PlayerCount ()
    , HasId (Maybe StoryEnemyId) CardCode env
    , HasId LocationId EnemyId env
    , HasQueue env
    , HasSet ActId env ()
    , HasSet ClosestLocationId env (LocationId, LocationId)
    , HasSet EnemyId env ()
    , HasSet EnemyId env LocationId
    , HasSet EnemyId env Trait
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env EnemyId
    , HasSet LocationId env ()
    , HasSet LocationId env [Trait]
    , HasSet UnengagedEnemyId env ()
    )

