module Arkham.Types.Enemy.Runner where

import Arkham.Types.AgendaId
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait
import ClassyPrelude

type EnemyRunner env
  = ( HasQueue env
    , HasCount env CardCount InvestigatorId
    , HasCount env PlayerCount ()
    , HasCount env RemainingSanity InvestigatorId
    , HasId (Maybe LocationId) env LocationName
    , HasId LeadInvestigatorId env ()
    , HasId LocationId env InvestigatorId
    , HasModifiersFor env env
    , HasSet ClosestLocationId env (LocationId, LocationId)
    , HasSet ClosestLocationId env (LocationId, Prey)
    , HasSet ConnectedLocationId env LocationId
    , HasSet EmptyLocationId env ()
    , HasSet FarthestLocationId env (InvestigatorId, EmptyLocation)
    , HasSet FarthestLocationId env InvestigatorId
    , HasSet FarthestLocationId env [InvestigatorId]
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env LocationId
    , HasSet LocationId env ()
    , HasSet LocationId env [Trait]
    , HasSet PreyId env (Prey, LocationId)
    , HasSet PreyId env Prey
    , HasSet Trait env EnemyId
    , HasSet Trait env LocationId
    , HasSource ForSkillTest env
    , HasStep AgendaStep env
    )
