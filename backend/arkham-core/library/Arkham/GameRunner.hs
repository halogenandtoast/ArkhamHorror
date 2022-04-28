module Arkham.GameRunner where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Card.Id
import Arkham.Classes
import Arkham.Enemy
import Arkham.Id
import Arkham.Keyword (Keyword)
import Arkham.Location
import Arkham.Matcher
import Arkham.Name
import Arkham.Prey
import Arkham.Query
import Arkham.SkillTest
import Arkham.Trait

type GameRunner env =
  ( HasQueue env
  , Query AssetMatcher env
  , ( HasCount ActsRemainingCount env ()
    , HasCount ActionTakenCount env InvestigatorId
    , HasCount CardCount env InvestigatorId
    , HasCount ClueCount env InvestigatorId
    , HasCount ClueCount env LocationId
    , HasCount DoomCount env ()
    , HasCount XPCount env ()
    , HasCount DoomCount env EnemyId
    , HasCount HealthDamageCount env EnemyId
    , HasCount HorrorCount env InvestigatorId
    , HasCount PlayerCount env ()
    , HasCount RemainingSanity env InvestigatorId
    , HasCount ResourceCount env InvestigatorId
    , HasCount SanityDamageCount env EnemyId
    , HasCount Shroud env LocationId
    , HasCount SpendableClueCount env InvestigatorId
    )
  , ( HasId (Maybe AssetId) env CardCode
    , HasId (Maybe OwnerId) env AssetId
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
    , HasSet VictoryDisplayCardCode env ()
    )
  , HasSkillTest env
  , HasAbilities env
  , HasRecord env ()
  , HasTokenValue env InvestigatorId
  , CanBeWeakness env TreacheryId
  , HasStep AgendaStep env ()
  )
