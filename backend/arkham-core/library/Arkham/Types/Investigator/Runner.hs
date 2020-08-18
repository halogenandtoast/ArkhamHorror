module Arkham.Types.Investigator.Runner where

import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AssetId
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.Trait
import Arkham.Types.Message
import Arkham.Types.TreacheryId

type InvestigatorRunner env
  = ( HasCount ClueCount LocationId env
    , HasCount AssetCount (InvestigatorId, [Trait]) env
    , HasSet HealthDamageableAssetId InvestigatorId env
    , HasSet SanityDamageableAssetId InvestigatorId env
    , HasSet CommitedCardId InvestigatorId env
    , HasQueue env
    , HasSet AdvanceableActId () env
    , HasSet ConnectedLocationId LocationId env
    , HasSet BlockedLocationId () env
    , HasSet EnemyId LocationId env
    , HasSet InvestigatorId EnemyId env
    , HasSet TreacheryId LocationId env
    , HasSet AloofEnemyId LocationId env
    , HasSet AssetId LocationId env
    , HasList Ability AssetId env
    , HasList Ability LocationId env
    , HasList Ability EnemyId env
    , HasList Ability TreacheryId env
    , HasList Ability () env
    , HasList UsedAbility () env
    , HasSet Trait AssetId env
    , HasId LocationId InvestigatorId env
    , HasList Message (LocationId, InvestigatorId) env
    )
