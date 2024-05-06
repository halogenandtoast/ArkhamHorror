{-# LANGUAGE TemplateHaskell #-}

module Arkham.Movement where

import Arkham.Id
import Arkham.Matcher
import {-# SOURCE #-} Arkham.Message (Message)
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
  , moveAfter :: [Message]
  }
  deriving stock (Show, Eq)

data MovementMeans = Direct | OneAtATime | Towards
  deriving stock (Show, Eq)

uncancellableMove :: Movement -> Movement
uncancellableMove m = m {moveCancelable = False}

afterMove :: [Message] -> Movement -> Movement
afterMove msgs m = m {moveAfter = msgs}

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
    , moveAfter = []
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
    , moveAfter = []
    }

moveToLocationMatcher :: Movement -> LocationMatcher
moveToLocationMatcher = destinationToLocationMatcher . moveDestination

destinationToLocationMatcher :: Destination -> LocationMatcher
destinationToLocationMatcher = \case
  ToLocation lid -> LocationWithId lid
  ToLocationMatching matcher -> matcher

$(deriveJSON defaultOptions ''MovementMeans)
$(deriveJSON defaultOptions ''Destination)

instance FromJSON Movement where
  parseJSON = withObject "Movement" $ \o -> do
    moveSource <- o .: "moveSource"
    moveTarget <- o .: "moveTarget"
    moveDestination <- o .: "moveDestination"
    moveMeans <- o .: "moveMeans"
    moveCancelable <- o .: "moveCancelable"
    movePayAdditionalCosts <- o .: "movePayAdditionalCosts"
    moveAfter <- o .:? "moveAfter" .!= []
    pure Movement {..}

$(deriveToJSON defaultOptions ''Movement)
