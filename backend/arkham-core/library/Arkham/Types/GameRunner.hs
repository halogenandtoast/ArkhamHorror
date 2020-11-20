module Arkham.Types.GameRunner where

import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.Asset.Uses (UseType)
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Enemy
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Keyword (Keyword)
import Arkham.Types.Location
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import ClassyPrelude

type GameRunner env
  = ( HasQueue env
    , ( HasCount env ActsRemainingCount ()
      , HasCount env ActionTakenCount InvestigatorId
      , HasCount env AssetCount (InvestigatorId, [Trait])
      , HasCount env CardCount InvestigatorId
      , HasCount env ClueCount InvestigatorId
      , HasCount env ClueCount LocationId
      , HasCount env DoomCount ()
      , HasCount env XPCount ()
      , HasCount env DoomCount EnemyId
      , HasCount env EnemyCount InvestigatorId
      , HasCount env EnemyCount (InvestigatorLocation, [Trait])
      , HasCount env EnemyCount [Trait]
      , HasCount env HealthDamageCount EnemyId
      , HasCount env HorrorCount InvestigatorId
      , HasCount env PlayerCount ()
      , HasCount env RemainingSanity InvestigatorId
      , HasCount env ResourceCount InvestigatorId
      , HasCount env SanityDamageCount EnemyId
      , HasCount env Shroud LocationId
      , HasCount env SpendableClueCount InvestigatorId
      , HasCount env TreacheryCount (LocationId, CardCode)
      )
    , ( HasId (Maybe AssetId) CardCode env
      , HasId (Maybe LocationId) LocationName env
      , HasId (Maybe OwnerId) AssetId env
      , HasId (Maybe StoryAssetId) CardCode env
      , HasId (Maybe StoryEnemyId) CardCode env
      , HasId ActiveInvestigatorId () env
      , HasId LeadInvestigatorId () env
      , HasId CardCode AssetId env
      , HasId CardCode EnemyId env
      , HasId LocationId EnemyId env
      , HasId LocationId InvestigatorId env
      )
    , ( HasList DeckCard (InvestigatorId, Trait) env
      , HasList Enemy () env
      , HasList Location () env
      , HasList UsedAbility () env
      , HasList LocationName () env
      , HasList HandCard InvestigatorId env
      )
    , HasModifiersFor env env
    , ( HasSet AccessibleLocationId env LocationId
      , HasSet AdvanceableActId env ()
      , HasSet AgendaId env ()
      , HasSet AloofEnemyId env LocationId
      , HasSet AssetId env (InvestigatorId, UseType)
      , HasSet AssetId env InvestigatorId
      , HasSet AssetId env LocationId
      , HasSet BlockedLocationId env ()
      , HasSet ClosestEnemyId env (LocationId, [Trait])
      , HasSet ClosestEnemyId env InvestigatorId
      , HasSet ClosestEnemyId env (InvestigatorId, [Trait])
      , HasSet ClosestLocationId env (LocationId, LocationId)
      , HasSet ClosestLocationId env (LocationId, Prey)
      , HasSet CommittedCardCode env ()
      , HasSet CommittedCardId env InvestigatorId
      , HasSet ConnectedLocationId env LocationId
      , HasSet DiscardableAssetId env InvestigatorId
      , HasSet EmptyLocationId env ()
      , HasSet EnemyId env ()
      , HasSet EnemyId env InvestigatorId
      , HasSet EnemyId env LocationId
      , HasSet EnemyId env Trait
      , HasSet EventId env ()
      , HasSet EventId env LocationId
      , HasSet ExhaustedAssetId env InvestigatorId
      , HasSet FarthestEnemyId env (InvestigatorId, EnemyTrait)
      , HasSet FarthestLocationId env (InvestigatorId, EmptyLocation)
      , HasSet FarthestLocationId env [InvestigatorId]
      , HasSet FarthestLocationId env InvestigatorId
      , HasSet HandCardId env InvestigatorId
      , HasSet HandCardId env (InvestigatorId, PlayerCardType)
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
      , HasSet HealthDamageableAssetId env InvestigatorId
      , HasSet SanityDamageableAssetId env InvestigatorId
      , HasSet Trait env AssetId
      , HasSet Trait env EnemyId
      , HasSet Trait env LocationId
      , HasSet TreacheryId env LocationId
      , HasSet UniqueEnemyId env ()
      , HasSet VictoryDisplayCardCode env ()
      )
    , HasSource ForSkillTest env
    , HasActions env ()
    , HasActions env AssetId
    , HasActions env (ActionType, Trait)
    , HasActions env ActionType
    , HasRecord env
    , HasTokenValue env InvestigatorId
    , CanBeWeakness TreacheryId env
    , HasStep AgendaStep env
    )
