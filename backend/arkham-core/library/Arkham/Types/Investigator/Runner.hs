module Arkham.Types.Investigator.Runner where

import Arkham.Types.ActId
import Arkham.Types.AssetId
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Enemy
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Location
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import ClassyPrelude

type InvestigatorRunner investigator env
  = ( HasCount ClueCount LocationId env
    , HasCount AssetCount (InvestigatorId, [Trait]) env
    , HasSet HealthDamageableAssetId InvestigatorId env
    , HasSet SanityDamageableAssetId InvestigatorId env
    , HasSet CommittedCardId InvestigatorId env
    , HasSet EventId () env
    , HasQueue env
    , HasSet AdvanceableActId () env
    , HasSet ConnectedLocationId LocationId env
    , HasSet BlockedLocationId () env
    , HasSet EnemyId LocationId env
    , HasSet InvestigatorId EnemyId env
    , HasSet TreacheryId LocationId env
    , HasSet AloofEnemyId LocationId env
    , HasSet AssetId LocationId env
    , HasSet Trait AssetId env
    , HasId LocationId InvestigatorId env
    , HasId (Maybe AssetId) CardCode env
    , HasList Location () env
    , HasList Enemy () env
    , HasActions env investigator env
    , HasActions env investigator (ActionType, Trait, env)
    )
