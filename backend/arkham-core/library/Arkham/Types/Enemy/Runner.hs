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
    , HasStep AgendaStep env
    , HasCount CardCount InvestigatorId env
    , HasCount PlayerCount () env
    , HasCount RemainingSanity InvestigatorId env
    , HasId LeadInvestigatorId () env
    , HasId LocationId InvestigatorId env
    , HasModifiers env LocationId
    , HasModifiersFor env env
    , HasSet ClosestLocationId (LocationId, LocationId) env
    , HasSet ClosestLocationId (LocationId, Prey) env
    , HasSet ConnectedLocationId LocationId env
    , HasSet EmptyLocationId () env
    , HasSet FarthestLocationId (InvestigatorId, EmptyLocation) env
    , HasSet FarthestLocationId [InvestigatorId] env
    , HasSet FarthestLocationId InvestigatorId env
    , HasSet InvestigatorId () env
    , HasSet InvestigatorId LocationId env
    , HasSet LocationId () env
    , HasSet LocationId [Trait] env
    , HasSet PreyId (Prey, LocationId) env
    , HasSet PreyId Prey env
    , HasSet Trait EnemyId env
    , HasSet Trait LocationId env
    , HasSource ForSkillTest env
    , HasId (Maybe LocationId) LocationName env
    )
