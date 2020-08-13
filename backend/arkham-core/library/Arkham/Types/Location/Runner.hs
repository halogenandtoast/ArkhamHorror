module Arkham.Types.Location.Runner where

import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Query
import Arkham.Types.Stats
import ClassyPrelude

type LocationRunner env
  = ( HasCount PlayerCount () env
    , HasQueue env
    , HasLog env
    , HasId StoryAssetId CardCode env
    , HasInvestigatorStats Stats InvestigatorId env
    , HasId (Maybe OwnerId) AssetId env
    )

