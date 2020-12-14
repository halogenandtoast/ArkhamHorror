module Arkham.Types.Treachery.Runner where

import Arkham.Import

import Arkham.Types.ScenarioLogKey
import Arkham.Types.Trait

type TreacheryRunner env
  = ( HasQueue env
    , HasCount ActsRemainingCount env ()
    , HasCount CardCount env InvestigatorId
    , HasCount ClueCount env InvestigatorId
    , HasCount ClueCount env LocationId
    , HasCount DamageCount env InvestigatorId
    , HasCount PlayerCount env ()
    , HasCount ResourceCount env InvestigatorId
    , HasCount Shroud env LocationId
    , HasCount SpendableClueCount env InvestigatorId
    , HasCount TreacheryCount env (LocationId, CardCode)
    , HasId (Maybe StoryEnemyId) env CardCode
    , HasId CardCode env AssetId
    , HasId LocationId env EnemyId
    , HasId LocationId env InvestigatorId
    , HasList UsedAbility env ()
    , HasPhaseHistory env
    , HasSet ActId env ()
    , HasSet ActId env TreacheryCardCode
    , HasSet AgendaId env ()
    , HasSet AgendaId env TreacheryCardCode
    , HasSet AssetId env InvestigatorId
    , HasSet ClosestEnemyId env (LocationId, [Trait])
    , HasSet ClosestPathLocationId env (LocationId, LocationId)
    , HasSet ConnectedLocationId env LocationId
    , HasSet DiscardableAssetId env InvestigatorId
    , HasSet EnemyId env Trait
    , HasSet FarthestLocationId env InvestigatorId
    , HasSet HandCardId env (InvestigatorId, PlayerCardType)
    , HasSet InvestigatorId env LocationId
    , HasSet InvestigatorId env TreacheryCardCode
    , HasSet InvestigatorId env ()
    , HasSet LocationId env ()
    , HasSet LocationId env TreacheryCardCode
    , HasSet LocationId env [Trait]
    , HasSet ScenarioLogKey env ()
    , HasSet StoryAssetId env InvestigatorId
    , HasSet UnengagedEnemyId env ()
    , HasSet Trait env LocationId
    , HasSet FarthestEnemyId env (InvestigatorId, EnemyTrait)
    , HasSet UniqueEnemyId env ()
    , HasSet EnemyId env ()
    )

