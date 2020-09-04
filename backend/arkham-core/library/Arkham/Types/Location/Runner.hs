module Arkham.Types.Location.Runner where

import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Stats
import Arkham.Types.Trait
import ClassyPrelude

type LocationRunner env
  = ( HasCount PlayerCount () env
    , HasQueue env
    , HasId (Maybe StoryAssetId) CardCode env
    , HasInvestigatorStats Stats InvestigatorId env
    , HasId (Maybe OwnerId) AssetId env
    , HasId CardCode EnemyId env
    , HasSet Trait LocationId env
    , HasSet EventId () env
    , HasSet Trait EnemyId env
    , HasList Modifier LocationId env
    , HasSet ConnectedLocationId LocationId env
    , HasId ActiveInvestigatorId () env
    )

