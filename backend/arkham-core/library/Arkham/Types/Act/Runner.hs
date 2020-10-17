module Arkham.Types.Act.Runner where

import Arkham.Import

import Arkham.Types.ScenarioLogKey
import Arkham.Types.Trait (Trait)

type ActRunner env
  = ( HasQueue env
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
    , HasId CardCode EnemyId env
    , HasId (Maybe StoryEnemyId) CardCode env
    , HasCount DamageCount EnemyId env
    , HasSet ScenarioLogKey () env
    , HasSet Trait AssetId env
    , HasSet AssetId EnemyId env
    )
