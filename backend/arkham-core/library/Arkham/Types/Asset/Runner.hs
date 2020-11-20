module Arkham.Types.Asset.Runner where

import ClassyPrelude

import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.Trait

type AssetRunner env
  = ( HasQueue env
    , HasModifiersFor env env
    , HasCount env AssetCount (InvestigatorId, [Trait])
    , HasCount env CardCount InvestigatorId
    , HasCount env ClueCount LocationId
    , HasCount env EnemyCount InvestigatorId
    , HasCount env HealthDamageCount EnemyId
    , HasCount env HorrorCount InvestigatorId
    , HasCount env ResourceCount InvestigatorId
    , HasCount env SanityDamageCount EnemyId
    , HasId (Maybe StoryEnemyId) CardCode env
    , HasId ActiveInvestigatorId () env
    , HasId CardCode EnemyId env
    , HasId LocationId InvestigatorId env
    , HasList DeckCard (InvestigatorId, Trait) env
    , HasSet AccessibleLocationId env LocationId
    , HasSet AssetId env InvestigatorId
    , HasSet BlockedLocationId env ()
    , HasSet ConnectedLocationId env LocationId
    , HasSet EnemyId env InvestigatorId
    , HasSet EnemyId env LocationId
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env LocationId
    , HasSet Trait env AssetId
    , HasSet Trait env EnemyId
    )
