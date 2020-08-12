module Arkham.Types.Agenda.Runner where

import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.EnemyId
import Arkham.Types.ActId
import Arkham.Types.Query
import Arkham.Types.Trait

type AgendaRunner env
  = ( HasId LeadInvestigatorId () env
    , HasCount PlayerCount () env
    , HasQueue env
    , HasSet UnengagedEnemyId () env
    , HasSet EnemyId Trait env
    , HasSet EnemyId LocationId env
    , HasSet ClosestLocationId (LocationId, LocationId) env
    , HasId LocationId EnemyId env
    , HasCount EnemyCount (LocationId, [Trait]) env
    , HasSet ActId () env
    , HasSet InvestigatorId () env
    )

