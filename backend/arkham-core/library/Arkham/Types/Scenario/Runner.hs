module Arkham.Types.Scenario.Runner where

import Arkham.Prelude

import Arkham.Types.AgendaId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.EnemyMatcher
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Query
import Arkham.Types.ScenarioId
import Arkham.Types.ScenarioLogKey
import Arkham.Types.SkillTest
import Arkham.Types.Trait

type ScenarioRunner env
  = ( HasCount DoomCount env ()
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
    , HasId (Maybe StoryEnemyId) env CardCode
    , HasList DeckCard env InvestigatorId
    , HasList CampaignStoryCard env ()
    , HasList UnderneathCard env AgendaId
    , HasName env LocationId
    , HasQueue env
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
    , HasSet StoryEnemyId env CardCode
    , HasSet Trait env LocationId
    , HasSet VictoryDisplayCardCode env ()
    , HasSkillTest env
    )

