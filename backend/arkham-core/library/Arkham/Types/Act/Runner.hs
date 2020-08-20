module Arkham.Types.Act.Runner where

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query

type ActRunner env
  = ( HasQueue env
    , HasCount SpendableClueCount AllInvestigators env
    , HasCount SpendableClueCount InvestigatorId env
    , HasSet EnemyId LocationId env
    , HasSet InvestigatorId LocationId env
    , HasCount PlayerCount () env
    , HasSet InvestigatorId () env
    , HasId LeadInvestigatorId () env
    )
