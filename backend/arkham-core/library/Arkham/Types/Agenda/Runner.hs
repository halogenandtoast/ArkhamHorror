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
    , HasSet ActId () env
    , HasSet ClosestLocationId (LocationId, LocationId) env
    , HasSet EnemyId () env
    , HasSet EnemyId LocationId env
    , HasSet EnemyId Trait env
    , HasSet InvestigatorId () env
    , HasSet InvestigatorId EnemyId env
    , HasSet LocationId () env
    , HasSet LocationId [Trait] env
    , HasSet UnengagedEnemyId () env
    )

