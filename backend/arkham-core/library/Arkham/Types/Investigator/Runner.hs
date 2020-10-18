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
import Arkham.Types.Keyword
import Arkham.Types.Location
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import ClassyPrelude

type InvestigatorRunner investigator env
  = ( HasCount ClueCount LocationId env
    , HasCount AssetCount (InvestigatorId, [Trait]) env
    , HasSet HealthDamageableAssetId InvestigatorId env
    , HasSet SanityDamageableAssetId InvestigatorId env
    , HasSet CommittedCardId InvestigatorId env
    , HasSet CommittedCardCode () env
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
    , HasSet Trait EnemyId env
    , HasId LocationId InvestigatorId env
    , HasId (Maybe AssetId) CardCode env
    , HasList Location () env
    , HasList Enemy () env
    , HasActions env env
    , HasActions env (ActionType, Trait, env)
    , HasSet InvestigatorId () env
    , HasSet ExhaustedAssetId InvestigatorId env
    , HasModifiersFor env env
    , HasSet HandCardId InvestigatorId env
    , HasSource ForSkillTest env
    , HasTestAction ForSkillTest env
    , HasSet Keyword EnemyId env
    )
