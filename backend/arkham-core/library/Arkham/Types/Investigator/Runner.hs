module Arkham.Types.Investigator.Runner where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.Action
import Arkham.Types.AgendaId
import Arkham.Types.Asset.Uses (UseType)
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Keyword
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.ScenarioLogKey
import Arkham.Types.SkillTest
import Arkham.Types.Source
import Arkham.Types.Trait
import Arkham.Types.TreacheryId

type InvestigatorRunner env
  = ( CanBeWeakness env TreacheryId
    , (HasActions env (), HasActions env AssetId, HasActions env ActionType)
    , ( HasCount ActionTakenCount env InvestigatorId
      , HasCount ActionRemainingCount env InvestigatorId
      , HasCount
          ActionRemainingCount
          env
          (Maybe Action, [Trait], InvestigatorId)
      , HasCount ActsRemainingCount env ()
      , HasCount AssetCount env (InvestigatorId, [Trait])
      , HasCount CardCount env InvestigatorId
      , HasCount ClueCount env InvestigatorId
      , HasCount ClueCount env LocationId
      , HasCount DamageCount env InvestigatorId
      , HasCount DiscardCount env InvestigatorId
      , HasCount EnemyCount env InvestigatorId
      , HasCount HealthDamageCount env EnemyId
      , HasCount HorrorCount env InvestigatorId
      , HasCount PlayerCount env ()
      , HasCount RemainingSanity env InvestigatorId
      , HasCount ResourceCount env InvestigatorId
      , HasCount SanityDamageCount env EnemyId
      , HasCount SetAsideCount env CardCode
      , HasCount Shroud env LocationId
      , HasCount SpendableClueCount env InvestigatorId
      , HasCount SpendableClueCount env ()
      , HasCount TreacheryCount env (LocationId, CardCode)
      , HasCount UsesCount env AssetId
      )
    , ( HasId (Maybe AssetId) env CardCode
      , HasId (Maybe LocationId) env (Direction, LocationId)
      , HasId (Maybe LocationId) env AssetId
      , HasId (Maybe LocationId) env LocationMatcher
      , HasId (Maybe OwnerId) env AssetId
      , HasId (Maybe StoryAssetId) env CardCode
      , HasId (Maybe StoryEnemyId) env CardCode
      , HasId ActiveInvestigatorId env ()
      , HasId CardCode env AssetId
      , HasId CardCode env EnemyId
      , HasId CardCode env LocationId
      , HasId LeadInvestigatorId env ()
      , HasId LocationId env EnemyId
      , HasId LocationId env InvestigatorId
      )
    , ( HasList DiscardedEncounterCard env ()
      , HasList DiscardableHandCard env InvestigatorId
      , HasList DiscardedPlayerCard env InvestigatorId
      , HasList HandCard env InvestigatorId
      , HasList InPlayCard env InvestigatorId
      , HasList LocationName env ()
      , HasList UsedAbility env ()
      )
    , HasModifiersFor env ()
    , ( HasName env AssetId
      , HasName env LocationId
      , HasName env SetAsideLocationCardCode
      )
    , HasPhaseHistory env
    , HasPlayerCard env AssetId
    , HasQueue env
    , HasRecord env
    , HasRoundHistory env
    , ( ( HasSet AccessibleLocationId env LocationId
        , HasSet ActId env TreacheryCardCode
        , HasSet ActId env ()
        , HasSet AgendaId env ()
        , HasSet AgendaId env TreacheryCardCode
        , HasSet AloofEnemyId env LocationId
        , HasSet AssetId env ()
        , HasSet AssetId env (InvestigatorId, UseType)
        , HasSet AssetId env (InvestigatorId, [Trait])
        , HasSet AssetId env InvestigatorId
        , HasSet AssetId env LocationId
        , HasSet BlockedLocationId env ()
        , HasSet ClosestEnemyId env (LocationId, [Trait])
        , HasSet ClosestLocationId env (LocationId, [Trait])
        , HasSet ClosestPathLocationId env (LocationId, LocationId)
        , HasSet ClosestPathLocationId env (LocationId, Prey)
        , HasSet CommittedCardCode env ()
        , HasSet CommittedCardId env InvestigatorId
        , HasSet ConnectedLocationId env LocationId
        , HasSet DiscardableAssetId env InvestigatorId
        , HasSet EmptyLocationId env ()
        , HasSet EnemyAccessibleLocationId env (EnemyId, LocationId)
        , HasSet EnemyId env CardCode
        , HasSet EnemyId env InvestigatorId
        , HasSet EnemyId env LocationId
        , HasSet EnemyId env Trait
        , HasSet EnemyId env ()
        , HasSet EnemyId env ([Trait], LocationId)
        , HasSet EventId env ()
        , HasSet ExhaustedAssetId env InvestigatorId
        , HasSet ExhaustedAssetId env ()
        , HasSet ExhaustedEnemyId env LocationId
        , HasSet FarthestEnemyId env (InvestigatorId, EnemyTrait)
        , HasSet FarthestLocationId env InvestigatorId
        , HasSet FarthestLocationId env [InvestigatorId]
        , HasSet FightableEnemyId env (InvestigatorId, Source)
        , HasSet HandCardId env (InvestigatorId, CardType)
        , HasSet HandCardId env InvestigatorId
        , HasSet HealthDamageableAssetId env InvestigatorId
        , HasSet InScenarioInvestigatorId env ()
        )
      , ( HasSet InvestigatorId env ()
        , HasSet InvestigatorId env EnemyId
        , HasSet InvestigatorId env LocationId
        , HasSet InvestigatorId env TreacheryCardCode
        , HasSet InvestigatorId env (HashSet LocationId)
        , HasSet Keyword env EnemyId
        , HasSet LocationId env LocationMatcher
        , HasSet LocationId env TreacheryCardCode
        , HasSet LocationId env (HashSet LocationSymbol)
        , HasSet LocationId env [Trait]
        , HasSet LocationId env ()
        , HasSet PreyId env Prey
        , HasSet PreyId env (Prey, LocationId)
        , HasSet RevealedLocationId env ()
        , HasSet SanityDamageableAssetId env InvestigatorId
        , HasSet SetAsideLocationCardCode env ()
        , HasSet ScenarioLogKey env ()
        , HasSet StoryAssetId env InvestigatorId
        , HasSet StoryEnemyId env CardCode
        , HasSet Trait env AssetId
        , HasSet Trait env EnemyId
        , HasSet Trait env LocationId
        , HasSet Trait env Source
        , HasSet Trait env (InvestigatorId, CardId)
        , HasSet TreacheryId env LocationId
        , HasSet UnengagedEnemyId env ()
        , HasSet UnengagedEnemyId env LocationId
        , HasSet UniqueEnemyId env ()
        , HasSet UnrevealedLocationId env ()
        , HasSet UnrevealedLocationId env LocationMatcher
        )
      )
    , (HasStep env ActStep, HasStep env AgendaStep)
    , HasSkillTest env
    )
