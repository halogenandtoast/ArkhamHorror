module Arkham.Types.Treachery.Runner where

import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query

type TreacheryRunner env
  = ( HasQueue env
    , HasLog env
    , HasSet AssetId InvestigatorId env
    , HasId LocationId InvestigatorId env
    , HasCount TreacheryCount (LocationId, CardCode) env
    )

