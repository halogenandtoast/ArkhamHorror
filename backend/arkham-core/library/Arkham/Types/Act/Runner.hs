module Arkham.Types.Act.Runner where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Token
import Arkham.Types.Trait (Trait)

type ActRunner env
  = ( HasQueue env
    , Query AssetMatcher env
    , HasCount ClueCount env AssetId
    , HasCount DamageCount env EnemyId
    , HasCount PlayerCount env ()
    , HasCount SpendableClueCount env InvestigatorId
    , HasCount SpendableClueCount env ()
    , HasId (Maybe LocationId) env LocationMatcher
    , HasId (Maybe OwnerId) env AssetId
    , HasId (Maybe StoryEnemyId) env CardCode
    , HasId CardCode env AssetId
    , HasId CardCode env EnemyId
    , HasId LeadInvestigatorId env ()
    , HasList Token env ()
    , HasList ResignedCardCode env ()
    , HasRecord env
    , HasSet CompletedScenarioId env ()
    , HasSet EnemyId env LocationId
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env (Set LocationId)
    , HasSet InvestigatorId env LocationId
    , HasSet InvestigatorId env LocationMatcher
    , HasSet InScenarioInvestigatorId env ()
    , HasSet LocationId env ()
    , HasSet LocationId env [Trait]
    , HasSet ScenarioLogKey env ()
    , HasSet Trait env AssetId
    , HasSet UnrevealedLocationId env ()
    , HasSet VictoryDisplayCardCode env ()
    , HasStep env AgendaStep
    , HasList LocationName env ()
    )
