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
    , ( HasSet AccessibleLocationId LocationId env
      , HasSet AdvanceableActId () env
      , HasSet AgendaId () env
      , HasSet AloofEnemyId LocationId env
      , HasSet AssetId (InvestigatorId, UseType) env
      , HasSet AssetId InvestigatorId env
      , HasSet AssetId LocationId env
      , HasSet BlockedLocationId () env
      , HasSet ClosestEnemyId (LocationId, [Trait]) env
      , HasSet ClosestEnemyId InvestigatorId env
      , HasSet ClosestEnemyId (InvestigatorId, [Trait]) env
      , HasSet ClosestLocationId (LocationId, LocationId) env
      , HasSet ClosestLocationId (LocationId, Prey) env
      , HasSet CommittedCardCode () env
      , HasSet CommittedCardId InvestigatorId env
      , HasSet ConnectedLocationId LocationId env
      , HasSet DiscardableAssetId InvestigatorId env
      , HasSet EmptyLocationId () env
      , HasSet EnemyId () env
      , HasSet EnemyId InvestigatorId env
      , HasSet EnemyId LocationId env
      , HasSet EnemyId Trait env
      , HasSet EventId () env
      , HasSet EventId LocationId env
      , HasSet ExhaustedAssetId InvestigatorId env
      , HasSet FarthestEnemyId (InvestigatorId, EnemyTrait) env
      , HasSet FarthestLocationId (InvestigatorId, EmptyLocation) env
      , HasSet FarthestLocationId [InvestigatorId] env
      , HasSet FarthestLocationId InvestigatorId env
      , HasSet HandCardId InvestigatorId env
      , HasSet HandCardId (InvestigatorId, PlayerCardType) env
      , HasSet InvestigatorId () env
      , HasSet InScenarioInvestigatorId () env
      , HasSet InvestigatorId EnemyId env
      , HasSet InvestigatorId LocationId env
      , HasSet Keyword EnemyId env
      , HasSet LocationId () env
      , HasSet LocationId [Trait] env
      , HasSet LocationId TreacheryCardCode env
      , HasSet PreyId (Prey, LocationId) env
      , HasSet PreyId Prey env
      , HasSet HealthDamageableAssetId InvestigatorId env
      , HasSet SanityDamageableAssetId InvestigatorId env
      , HasSet Trait AssetId env
      , HasSet Trait EnemyId env
      , HasSet Trait LocationId env
      , HasSet TreacheryId LocationId env
      , HasSet UniqueEnemyId () env
      , HasSet VictoryDisplayCardCode () env
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
