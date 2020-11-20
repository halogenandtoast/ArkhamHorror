module Arkham.Types.Investigator.Runner where

import Arkham.Types.ActId
import Arkham.Types.Asset.Uses (UseType)
import Arkham.Types.AssetId
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Keyword
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import ClassyPrelude

type InvestigatorRunner env
  = ( CanBeWeakness env TreacheryId
    , HasActions env ()
    , HasActions env AssetId
    , HasCount ActionTakenCount env InvestigatorId
    , HasCount AssetCount env (InvestigatorId, [Trait])
    , HasCount ClueCount env LocationId
    , HasId (Maybe AssetId) env CardCode
    , HasId CardCode env AssetId
    , HasId CardCode env EnemyId
    , HasId LocationId env InvestigatorId
    , HasModifiersFor env env
    , HasQueue env
    , HasSet AdvanceableActId env ()
    , HasSet AloofEnemyId env LocationId
    , HasSet AssetId env (InvestigatorId, UseType)
    , HasSet AssetId env LocationId
    , HasSet BlockedLocationId env ()
    , HasSet CommittedCardCode env ()
    , HasSet CommittedCardId env InvestigatorId
    , HasSet ConnectedLocationId env LocationId
    , HasSet DiscardableAssetId env InvestigatorId
    , HasSet EnemyId env LocationId
    , HasSet EventId env ()
    , HasSet ExhaustedAssetId env InvestigatorId
    , HasSet HandCardId env InvestigatorId
    , HasSet HealthDamageableAssetId env InvestigatorId
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env EnemyId
    , HasSet Keyword env EnemyId
    , HasSet SanityDamageableAssetId env InvestigatorId
    , HasSet Trait env AssetId
    , HasSet Trait env EnemyId
    , HasSet TreacheryId env LocationId
    , HasSource ForSkillTest env
    )
