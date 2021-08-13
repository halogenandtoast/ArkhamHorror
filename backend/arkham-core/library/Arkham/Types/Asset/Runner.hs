module Arkham.Types.Asset.Runner where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Query
import Arkham.Types.SkillTest
import Arkham.Types.Source
import Arkham.Types.Trait

type AssetRunner env
  = ( HasQueue env
    , HasSkillTest env
    , Query AssetMatcher env
    , HasCostPayment env
    , HasModifiersFor env ()
    , HasList UsedAbility env ()
    , HasCount ActionRemainingCount env InvestigatorId
    , HasCount AssetCount env (InvestigatorId, [Trait])
    , HasCount CardCount env InvestigatorId
    , HasCount ClueCount env LocationId
    , HasCount EnemyCount env InvestigatorId
    , HasCount HealthDamageCount env EnemyId
    , HasCount HorrorCount env InvestigatorId
    , HasCount ResourceCount env InvestigatorId
    , HasCount SanityDamageCount env EnemyId
    , HasId (Maybe LocationId) env (Direction, LocationId)
    , HasId (Maybe LocationId) env LocationMatcher
    , HasId (Maybe StoryEnemyId) env CardCode
    , HasId ActiveInvestigatorId env ()
    , HasId CardCode env EnemyId
    , HasId LocationId env InvestigatorId
    , HasRecord env
    , HasSet AccessibleLocationId env LocationId
    , HasSet BlockedLocationId env ()
    , HasSet ConnectedLocationId env LocationId
    , HasSet EnemyId env ([Trait], LocationId)
    , HasSet EnemyId env InvestigatorId
    , HasSet EnemyId env LocationId
    , HasSet ExhaustedEnemyId env LocationId
    , HasSet InScenarioInvestigatorId env ()
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env LocationId
    , HasSet StoryEnemyId env CardCode
    , HasSet Trait env AssetId
    , HasSet Trait env EnemyId
    , HasSet Trait env Source
    )
