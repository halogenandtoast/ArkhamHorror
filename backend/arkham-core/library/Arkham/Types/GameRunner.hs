module Arkham.Types.GameRunner where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Enemy
import Arkham.Types.Id
import Arkham.Types.Keyword (Keyword)
import Arkham.Types.Location
import Arkham.Types.Matcher
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.SkillTest
import Arkham.Types.Trait

type GameRunner env
  = ( HasQueue env
    , Query AssetMatcher env
    , ( HasCount ActsRemainingCount env ()
      , HasCount ActionTakenCount env InvestigatorId
      , HasCount AssetCount env (InvestigatorId, [Trait])
      , HasCount CardCount env InvestigatorId
      , HasCount ClueCount env InvestigatorId
      , HasCount ClueCount env LocationId
      , HasCount DoomCount env ()
      , HasCount XPCount env ()
      , HasCount DoomCount env EnemyId
      , HasCount EnemyCount env InvestigatorId
      , HasCount EnemyCount env (InvestigatorLocation, [Trait])
      , HasCount EnemyCount env [Trait]
      , HasCount HealthDamageCount env EnemyId
      , HasCount HorrorCount env InvestigatorId
      , HasCount PlayerCount env ()
      , HasCount RemainingSanity env InvestigatorId
      , HasCount ResourceCount env InvestigatorId
      , HasCount SanityDamageCount env EnemyId
      , HasCount Shroud env LocationId
      , HasCount SpendableClueCount env InvestigatorId
      , HasCount TreacheryCount env (LocationId, CardCode)
      )
    , ( HasId (Maybe AssetId) env CardCode
      , HasId (Maybe OwnerId) env AssetId
      , HasId (Maybe StoryEnemyId) env CardCode
      , HasId ActiveInvestigatorId env ()
      , HasId LeadInvestigatorId env ()
      , HasId CardCode env AssetId
      , HasId CardCode env EnemyId
      , HasId LocationId env EnemyId
      , HasId LocationId env InvestigatorId
      )
    , ( HasList Enemy env ()
      , HasList Location env ()
      , HasList UsedAbility env ()
      , HasList LocationName env ()
      , HasList HandCard env InvestigatorId
      )
    , HasModifiersFor env ()
    , ( HasSet AccessibleLocationId env LocationId
      , HasSet AgendaId env ()
      , HasSet AloofEnemyId env LocationId
      , HasSet BlockedLocationId env ()
      , HasSet ClosestEnemyId env (LocationId, [Trait])
      , HasSet ClosestEnemyId env InvestigatorId
      , HasSet ClosestEnemyId env (InvestigatorId, [Trait])
      , HasSet ClosestPathLocationId env (LocationId, LocationId)
      , HasSet ClosestPathLocationId env (LocationId, Prey)
      , HasSet CommittedCardCode env ()
      , HasSet CommittedCardId env InvestigatorId
      , HasSet ConnectedLocationId env LocationId
      , HasSet EmptyLocationId env ()
      , HasSet EnemyId env ()
      , HasSet EnemyId env InvestigatorId
      , HasSet EnemyId env LocationId
      , HasSet EnemyId env Trait
      , HasSet EventId env ()
      , HasSet EventId env LocationId
      , HasSet FarthestEnemyId env (InvestigatorId, EnemyTrait)
      , HasSet FarthestLocationId env [InvestigatorId]
      , HasSet FarthestLocationId env InvestigatorId
      , HasSet HandCardId env InvestigatorId
      , HasSet HandCardId env (InvestigatorId, CardType)
      , HasSet InvestigatorId env ()
      , HasSet InScenarioInvestigatorId env ()
      , HasSet InvestigatorId env EnemyId
      , HasSet InvestigatorId env LocationId
      , HasSet Keyword env EnemyId
      , HasSet LocationId env ()
      , HasSet LocationId env [Trait]
      , HasSet LocationId env TreacheryCardCode
      , HasSet PreyId env (Prey, LocationId)
      , HasSet PreyId env Prey
      , HasSet Trait env AssetId
      , HasSet Trait env EnemyId
      , HasSet Trait env LocationId
      , HasSet TreacheryId env LocationId
      , HasSet UniqueEnemyId env ()
      , HasSet VictoryDisplayCardCode env ()
      )
    , HasSkillTest env
    , HasAbilities env
    , HasRecord env
    , HasTokenValue env InvestigatorId
    , CanBeWeakness env TreacheryId
    , HasStep AgendaStep env ()
    )
