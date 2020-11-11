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
    , HasCount AssetCount (InvestigatorId, [Trait]) env
    , HasCount CardCount InvestigatorId env
    , HasCount ClueCount LocationId env
    , HasCount EnemyCount InvestigatorId env
    , HasCount HealthDamageCount EnemyId env
    , HasCount HorrorCount InvestigatorId env
    , HasCount ResourceCount InvestigatorId env
    , HasCount SanityDamageCount EnemyId env
    , HasId (Maybe StoryEnemyId) CardCode env
    , HasId ActiveInvestigatorId () env
    , HasId CardCode EnemyId env
    , HasId LocationId InvestigatorId env
    , HasList DeckCard (InvestigatorId, Trait) env
    , HasModifiers env InvestigatorId
    , HasSet AccessibleLocationId LocationId env
    , HasSet AssetId InvestigatorId env
    , HasSet BlockedLocationId () env
    , HasSet ConnectedLocationId LocationId env
    , HasSet EnemyId InvestigatorId env
    , HasSet EnemyId LocationId env
    , HasSet InvestigatorId () env
    , HasSet InvestigatorId LocationId env
    , HasSet Trait AssetId env
    , HasSet Trait EnemyId env
    )
