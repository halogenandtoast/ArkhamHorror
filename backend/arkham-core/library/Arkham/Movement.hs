module Arkham.Movement where

import Arkham.Prelude

import Arkham.Classes.Entity.Source
import Arkham.Source
import Arkham.Matcher
import Arkham.Target
import Arkham.Id

data Movement = Movement
  { moveSource :: Source
  , moveTarget :: Target
  , moveDestination :: Destination
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

data Destination = ToLocation LocationId | ToLocationMatching LocationMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

move :: Sourceable source => source -> InvestigatorId -> LocationId -> Movement
move (toSource -> source) iid lid = Movement
  { moveSource = source
  , moveTarget = InvestigatorTarget iid
  , moveDestination = ToLocation lid
  , moveMeans = Direct
  , moveCancelable = True
  }

moveToMatch :: Sourceable source => source -> InvestigatorId -> LocationMatcher -> Movement
moveToMatch (toSource -> source) iid matcher = Movement
  { moveSource = source
  , moveTarget = InvestigatorTarget iid
  , moveDestination = ToLocationMatching matcher
  , moveMeans = Direct
  , moveCancelable = True
  }

moveToLocationMatcher :: Movement -> LocationMatcher
moveToLocationMatcher = destinationToLocationMatcher . moveDestination

destinationToLocationMatcher :: Destination -> LocationMatcher
destinationToLocationMatcher = \case
  ToLocation lid -> LocationWithId lid
  ToLocationMatching matcher -> matcher
