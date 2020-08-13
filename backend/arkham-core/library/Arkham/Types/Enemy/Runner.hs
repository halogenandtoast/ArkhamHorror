module Arkham.Types.Enemy.Runner where

import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Prey
import Arkham.Types.Query

type EnemyRunner env
  = ( HasSet LocationId () env
    , HasSet InvestigatorId LocationId env
    , HasCount PlayerCount () env
    , HasQueue env
    , HasLog env
    , HasSet ClosestLocationId (LocationId, Prey) env
    , HasSet PreyId (Prey, LocationId) env
    , HasId LeadInvestigatorId () env
    )
