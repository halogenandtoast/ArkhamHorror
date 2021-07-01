module Arkham.Types.Treachery.Runner where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query


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
    , HasCount SetAsideCount env CardCode
    , HasCount Shroud env LocationId
    , HasCount SpendableClueCount env InvestigatorId
    , HasCount TreacheryCount env (LocationId, CardCode)
    , HasId (Maybe StoryEnemyId) env CardCode
    , HasId CardCode env AssetId
    , HasId CardCode env EnemyId
    , HasId LocationId env EnemyId
    , HasId LocationId env InvestigatorId
    , HasList UsedAbility env ()
    , HasList DiscardedPlayerCard env InvestigatorId
    , HasPhaseHistory env
    , HasSet ActId env ()
    , HasSet ActId env TreacheryCardCode
    , HasSet AgendaId env ()
    , HasSet AgendaId env TreacheryCardCode
    , HasSet AssetId env InvestigatorId
    , HasSet AssetId env (InvestigatorId, [Trait])
    , HasSet ClosestEnemyId env (LocationId, [Trait])
    , HasSet ClosestPathLocationId env (LocationId, LocationId)
    , HasSet ConnectedLocationId env LocationId
    , HasSet DiscardableAssetId env InvestigatorId
    , HasSet EnemyId env LocationId
    , HasSet EnemyId env Trait
    , HasSet EnemyId env CardCode
    , HasSet EnemyId env ([Trait], LocationId)
    , HasSet FarthestLocationId env InvestigatorId
    , HasSet HandCardId env (InvestigatorId, CardType)
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
