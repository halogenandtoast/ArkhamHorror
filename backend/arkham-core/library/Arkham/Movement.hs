{-# LANGUAGE TemplateHaskell #-}
module Arkham.Movement where

import Arkham.Prelude

import Arkham.Classes.Entity.Source
import Arkham.Source
import Arkham.Matcher
import Arkham.Target
import Arkham.Id
import Data.Aeson.TH

data Movement = Movement
  { moveSource :: Source
  , moveTarget :: Target
  , moveDestination :: Destination
  , moveMeans :: MovementMeans
  , moveCancelable :: Bool
  }
  deriving stock (Show, Eq)

data MovementMeans = Direct
  deriving stock (Show, Eq)

uncancellableMove :: Movement -> Movement
uncancellableMove m = m { moveCancelable = False }

data Destination = ToLocation LocationId | ToLocationMatching LocationMatcher
  deriving stock (Show, Eq)

move :: (Targetable target, Sourceable source) => source -> target -> LocationId -> Movement
move (toSource -> source) (toTarget -> target) lid = Movement
  { moveSource = source
  , moveTarget = target
  , moveDestination = ToLocation lid
  , moveMeans = Direct
  , moveCancelable = True
  }

moveToMatch :: (Targetable target, Sourceable source) => source -> target -> LocationMatcher -> Movement
moveToMatch (toSource -> source) (toTarget -> target) matcher = Movement
  { moveSource = source
  , moveTarget = target
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

$(deriveJSON defaultOptions ''MovementMeans)
$(deriveJSON defaultOptions ''Destination)
$(deriveJSON defaultOptions ''Movement)
