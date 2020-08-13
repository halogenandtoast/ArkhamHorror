module Arkham.Types.Asset.Runner where

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.Trait

type AssetRunner env
  = ( HasQueue env
    , HasLog env
    , HasSet InvestigatorId () env
    , HasSet InvestigatorId LocationId env
    , HasSet EnemyId LocationId env
    , HasId LocationId InvestigatorId env
    , HasCount EnemyCount InvestigatorId env
    , HasCount ClueCount LocationId env
    , HasCount ResourceCount InvestigatorId env
    , HasList DeckCard (InvestigatorId, Trait) env
    )
