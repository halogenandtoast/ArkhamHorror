module Arkham.Types.Enemy.Runner where

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait

type EnemyRunner env
  = ( HasQueue env
    , HasCount CardCount InvestigatorId env
    , HasCount PlayerCount () env
    , HasCount RemainingSanity InvestigatorId env
    , HasId LeadInvestigatorId () env
    , HasId LocationId InvestigatorId env
    , HasModifiers env LocationId
    , HasModifiersFor env env
    , HasSet ClosestLocationId (LocationId, LocationId) env
    , HasSet ClosestLocationId (LocationId, Prey) env
    , HasSet EmptyLocationId () env
    , HasSet InvestigatorId () env
    , HasSet InvestigatorId LocationId env
    , HasSet LocationId () env
    , HasSet LocationId [Trait] env
    , HasSet PreyId (Prey, LocationId) env
    , HasSet PreyId Prey env
    , HasSet Trait EnemyId env
    , HasSet Trait LocationId env
    , HasSource ForSkillTest env
    , HasTestAction ForSkillTest env
    )
