module Arkham.Types.Scenario.Runner where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Query
import Arkham.Types.ScenarioLogKey
import Arkham.Types.SkillTest
import Arkham.Types.Trait

type ScenarioRunner env
  = ( HasQueue env
    , Query EnemyMatcher env
    , Query InvestigatorMatcher env
    , HasCount DoomCount env ()
    , HasCount HorrorCount env LocationId
    , HasModifiersFor env ()
    , HasCount DoomCount env EnemyId
    , HasCount EnemyCount env [Trait]
    , HasCount PlayerCount env ()
    , HasCount ResourceCount env InvestigatorId
    , HasCount XPCount env ()
    , HasCount EnemyCount env (InvestigatorLocation, [Trait])
    , HasId CardCode env EnemyId
    , HasId LeadInvestigatorId env ()
    , HasId LocationId env InvestigatorId
    , HasId (Maybe CampaignId) env ()
    , HasId (Maybe EnemyId) env EnemyMatcher
    , HasList DeckCard env InvestigatorId
    , HasList CampaignStoryCard env ()
    , HasName env LocationId
    , HasRecord env
    , HasSet AgendaId env ()
    , HasSet ClosestEnemyId env (InvestigatorId, [Trait])
    , HasSet ClosestEnemyId env InvestigatorId
    , HasSet CompletedScenarioId env ()
    , HasSet ConnectedLocationId env LocationId
    , HasSet DefeatedInvestigatorId env ()
    , HasSet EnemyId env LocationId
    , HasSet EnemyId env Trait
    , HasSet InScenarioInvestigatorId env ()
    , HasSet InvestigatorId env ()
    , HasSet LocationId env ()
    , HasSet LocationId env [Trait]
    , HasSet LocationId env LocationMatcher
    , HasSet ScenarioLogKey env ()
    , HasSet Trait env LocationId
    , HasSet VictoryDisplayCardCode env ()
    , HasSkillTest env
    , Query LocationMatcher env
    )

