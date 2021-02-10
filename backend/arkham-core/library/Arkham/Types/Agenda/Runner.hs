module Arkham.Types.Agenda.Runner where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Trait

type AgendaRunner env
  = ( HasQueue env
    , HasCount ClueCount env InvestigatorId
    , HasCount ClueCount env LocationId
    , HasCount DiscardCount env InvestigatorId
    , HasCount DoomCount env ()
    , HasCount EnemyCount env (LocationId, [Trait])
    , HasCount EnemyCount env (LocationMatcher, [Trait])
    , HasCount PlayerCount env ()
    , HasCount ScenarioDeckCount env ()
    , HasId (Maybe LocationId) env LocationMatcher
    , HasId (Maybe LocationId) env (Direction, LocationId)
    , HasId (Maybe StoryEnemyId) env CardCode
    , HasId (Maybe StoryTreacheryId) env CardCode
    , HasId CardCode env EnemyId
    , HasId LeadInvestigatorId env ()
    , HasId LocationId env EnemyId
    , HasId LocationId env InvestigatorId
    , HasList LocationName env ()
    , HasSet ActId env ()
    , HasSet ClosestPathLocationId env (LocationId, LocationId)
    , HasSet ClosestPathLocationId env (LocationId, LocationMatcher)
    , HasSet CompletedScenarioId env ()
    , HasSet EnemyId env ()
    , HasSet EnemyId env ([Trait], LocationId)
    , HasSet EnemyId env LocationId
    , HasSet EnemyId env LocationMatcher
    , HasSet EnemyId env Trait
    , HasSet InScenarioInvestigatorId env ()
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env EnemyId
    , HasSet InvestigatorId env LocationMatcher
    , HasSet InvestigatorId env LocationId
    , HasSet LocationId env ()
    , HasSet LocationId env [Trait]
    , HasSet Trait env EnemyId
    , HasSet UnengagedEnemyId env ()
    )

