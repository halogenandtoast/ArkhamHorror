module Arkham.Types.Act.Runner where

import Arkham.Import

import Arkham.Types.Trait

type ActRunner env
  = ( HasQueue env
    , HasCount SpendableClueCount AllInvestigators env
    , HasCount SpendableClueCount InvestigatorId env
    , HasSet EnemyId LocationId env
    , HasSet InvestigatorId LocationId env
    , HasCount PlayerCount () env
    , HasSet InvestigatorId () env
    , HasId LeadInvestigatorId () env
    , HasSet VictoryDisplayCardCode () env
    , HasRecord env
    , HasSet LocationId () env
    , HasSet LocationId [Trait] env
    , HasSet InvestigatorId (HashSet LocationId) env
    )
