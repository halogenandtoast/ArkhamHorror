{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Runner where

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query

type EventRunner env
  = ( HasQueue env
    , HasId LocationId InvestigatorId env
    , HasCount ClueCount LocationId env
    , HasSet InvestigatorId LocationId env
    , HasSet EnemyId LocationId env
    , HasSet ConnectedLocationId LocationId env
    )
