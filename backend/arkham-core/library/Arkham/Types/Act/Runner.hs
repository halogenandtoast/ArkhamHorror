module Arkham.Types.Act.Runner where

import Arkham.Import

import Arkham.Types.ScenarioLogKey
import Arkham.Types.Trait (Trait)

type ActRunner env
  = ( HasQueue env
    , HasCount env DamageCount EnemyId
    , HasCount env PlayerCount ()
    , HasCount env SpendableClueCount InvestigatorId
    , HasId (Maybe LocationId) LocationName env
    , HasId (Maybe StoryEnemyId) CardCode env
    , HasId CardCode EnemyId env
    , HasId LeadInvestigatorId () env
    , HasRecord env
    , HasSet AssetId EnemyId env
    , HasSet EnemyId LocationId env
    , HasSet InvestigatorId () env
    , HasSet InvestigatorId (HashSet LocationId) env
    , HasSet InvestigatorId LocationId env
    , HasSet InvestigatorId LocationName env
    , HasSet LocationId () env
    , HasSet LocationId [Trait] env
    , HasSet ScenarioLogKey () env
    , HasSet Trait AssetId env
    , HasSet VictoryDisplayCardCode () env
    )
