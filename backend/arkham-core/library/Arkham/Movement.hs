{-# LANGUAGE TemplateHaskell #-}

module Arkham.Movement where

import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH

data Movement = Movement
  { moveSource :: Source
  , moveTarget :: Target
  , moveDestination :: Destination
  , moveMeans :: MovementMeans
  , moveCancelable :: Bool
  , movePayAdditionalCosts :: Bool
  }
  deriving stock (Show, Eq)

data MovementMeans = Direct
  deriving stock (Show, Eq)

uncancellableMove :: Movement -> Movement
uncancellableMove m = m {moveCancelable = False}

data Destination = ToLocation LocationId | ToLocationMatching LocationMatcher
  deriving stock (Show, Eq)

move
  :: (Targetable target, Sourceable source)
  => source
  -> target
  -> LocationId
  -> Movement
move (toSource -> moveSource) (toTarget -> moveTarget) lid =
  Movement
    { moveSource
    , moveTarget
    , moveDestination = ToLocation lid
    , moveMeans = Direct
    , moveCancelable = True
    , movePayAdditionalCosts = True
    }

moveToMatch
  :: (Targetable target, Sourceable source)
  => source
  -> target
  -> LocationMatcher
  -> Movement
moveToMatch (toSource -> moveSource) (toTarget -> moveTarget) matcher =
  Movement
    { moveSource
    , moveTarget
    , moveDestination = ToLocationMatching matcher
    , moveMeans = Direct
    , moveCancelable = True
    , movePayAdditionalCosts = True
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
