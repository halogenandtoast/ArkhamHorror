module Arkham.Types.Location.Runner where

import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
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
    )

