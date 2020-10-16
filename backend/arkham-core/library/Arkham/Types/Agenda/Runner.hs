module Arkham.Types.Agenda.Runner where

import Arkham.Import

import Arkham.Types.Trait

type AgendaRunner env
  = ( HasId LeadInvestigatorId () env
    , HasCount PlayerCount () env
    , HasQueue env
    , HasSet EnemyId () env
    , HasSet UnengagedEnemyId () env
    , HasSet EnemyId Trait env
    , HasSet EnemyId LocationId env
    , HasSet ClosestLocationId (LocationId, LocationId) env
    , HasId LocationId EnemyId env
    , HasCount EnemyCount (LocationId, [Trait]) env
    , HasSet ActId () env
    , HasSet InvestigatorId () env
    , HasCount ClueCount InvestigatorId env
    , HasCount DoomCount () env
    , HasId (Maybe StoryEnemyId) CardCode env
    , HasSet LocationId [Trait] env
    , HasSet LocationId () env
    , HasCount ClueCount LocationId env
    , HasSet InvestigatorId EnemyId env
    )

