module Arkham.Scenarios.TheEssexCountyExpress.Helpers where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.Id

leftmostLocation
  :: ( MonadReader env m
     , HasId (Maybe LocationId) env (Direction, LocationId)
     , MonadIO m
     )
  => LocationId
  -> m LocationId
leftmostLocation lid = do
  mlid' <- getId (LeftOf, lid)
  maybe (pure lid) leftmostLocation mlid'
