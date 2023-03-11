module Arkham.Movement where

import Arkham.Prelude

import Arkham.Classes.Entity.Source
import Arkham.Source
import Arkham.Target
import Arkham.Id

data Movement = Movement
  { moveSource :: Source
  , moveTarget :: Target
  , moveDestination :: LocationId
  , moveMeans :: MovementMeans
  , moveCancelable :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data MovementMeans = Direct
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

uncancellableMove :: Movement -> Movement
uncancellableMove m = m { moveCancelable = False }

move :: SourceEntity source => source -> InvestigatorId -> LocationId -> Movement
move (toSource -> source) iid lid = Movement
  { moveSource = source
  , moveTarget = InvestigatorTarget iid
  , moveDestination = lid
  , moveMeans = Direct
  , moveCancelable = True
  }
