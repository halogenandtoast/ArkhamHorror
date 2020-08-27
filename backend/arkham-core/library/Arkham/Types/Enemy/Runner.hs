module Arkham.Types.Enemy.Runner where

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Trait

type EnemyRunner env
  = ( HasSet LocationId () env
    , HasSet InvestigatorId LocationId env
    , HasSet InvestigatorId () env
    , HasCount PlayerCount () env
    , HasCount RemainingSanity InvestigatorId env
    , HasCount CardCount InvestigatorId env
    , HasQueue env
    , HasSet ClosestLocationId (LocationId, Prey) env
    , HasSet PreyId (Prey, LocationId) env
    , HasId LeadInvestigatorId () env
    , HasSet EmptyLocationId () env
    , HasId LocationId InvestigatorId env
    , HasSet PreyId Prey env
    , HasSet Trait EnemyId env
    , HasList Modifier LocationId env
    )
