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
    , ( HasCount ActionTakenCount InvestigatorId env
      , HasCount AssetCount (InvestigatorId, [Trait]) env
      , HasCount CardCount InvestigatorId env
      , HasCount ClueCount InvestigatorId env
      , HasCount ClueCount LocationId env
      , HasCount DoomCount () env
      , HasCount XPCount () env
      , HasCount DoomCount EnemyId env
      , HasCount EnemyCount InvestigatorId env
      , HasCount EnemyCount (InvestigatorLocation, [Trait]) env
      , HasCount EnemyCount [Trait] env
      , HasCount HealthDamageCount EnemyId env
      , HasCount HorrorCount InvestigatorId env
      , HasCount PlayerCount () env
      , HasCount RemainingSanity InvestigatorId env
      , HasCount ResourceCount InvestigatorId env
      , HasCount SanityDamageCount EnemyId env
      , HasCount Shroud LocationId env
      , HasCount SpendableClueCount InvestigatorId env
      , HasCount TreacheryCount (LocationId, CardCode) env
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
    , (HasModifiers env InvestigatorId, HasModifiers env LocationId)
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
      , HasSet EnemyId InvestigatorId env
      , HasSet EnemyId LocationId env
      , HasSet EnemyId Trait env
      , HasSet EventId () env
      , HasSet EventId LocationId env
      , HasSet ExhaustedAssetId InvestigatorId env
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
      , HasSet VictoryDisplayCardCode () env
      )
    , HasSource ForSkillTest env
    , HasActions env env
    , HasActions env AssetId
    , HasActions env (ActionType, Trait, env)
    , HasActions env (ActionType, env)
    , HasRecord env
    , HasTokenValue env InvestigatorId
    , CanBeWeakness TreacheryId env
    , HasStep AgendaStep env
    )
