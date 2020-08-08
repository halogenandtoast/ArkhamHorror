module Arkham.Types.Treachery.Runner where

import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId

type TreacheryRunner env
  = ( HasQueue env
    , HasSet AssetId InvestigatorId env
    , HasId LocationId InvestigatorId env
    )

