{-# LANGUAGE TemplateHaskell #-}

module Arkham.Movement where

import Arkham.Cost
import Arkham.Id
import Arkham.Matcher
import {-# SOURCE #-} Arkham.Message (Message)
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH
import Data.UUID (fromWords64)
import GHC.Records

newtype MoveId = MoveId UUID
  deriving stock (Show, Eq, Ord, Data)
  deriving newtype (ToJSON, FromJSON, Random)

data Movement = Movement
  { moveSource :: Source
  , moveTarget :: Target
  , moveDestination :: Destination
  , moveMeans :: MovementMeans
  , moveCancelable :: Bool
  , movePayAdditionalCosts :: Bool
  , moveAfter :: [Message]
  , moveAdditionalEnterCosts :: Cost
  , moveId :: MoveId
  }
  deriving stock (Show, Eq, Data)

instance HasField "source" Movement Source where
  getField = moveSource

instance HasField "target" Movement Target where
  getField = moveTarget

instance HasField "destination" Movement Destination where
  getField = moveDestination

instance HasField "means" Movement MovementMeans where
  getField = moveMeans

instance HasField "cancelable" Movement Bool where
  getField = moveCancelable

instance HasField "payAdditionalCosts" Movement Bool where
  getField = movePayAdditionalCosts

instance HasField "after" Movement [Message] where
  getField = moveAfter

instance HasField "additionalEnterCosts" Movement Cost where
  getField = moveAdditionalEnterCosts

data MovementMeans = Direct | OneAtATime | Towards | Place | TowardsN Int
  deriving stock (Show, Eq, Data)

-- Forced movement should not require additional costs
uncancellableMove :: Movement -> Movement
uncancellableMove m = m {moveCancelable = False, movePayAdditionalCosts = False}

afterMove :: [Message] -> Movement -> Movement
afterMove msgs m = m {moveAfter = msgs}

data Destination = ToLocation LocationId | ToLocationMatching LocationMatcher
  deriving stock (Show, Eq, Data)

move
  :: (MonadRandom m, Targetable target, Sourceable source)
  => source
  -> target
  -> LocationId
  -> m Movement
move (toSource -> moveSource) (toTarget -> moveTarget) lid = do
  moveId <- getRandom
  pure
    Movement
      { moveSource
      , moveTarget
      , moveDestination = ToLocation lid
      , moveMeans = Direct
      , moveCancelable = True
      , movePayAdditionalCosts = True
      , moveAfter = []
      , moveAdditionalEnterCosts = Free
      , moveId
      }

moveToMatch
  :: (MonadRandom m, Targetable target, Sourceable source)
  => source
  -> target
  -> LocationMatcher
  -> m Movement
moveToMatch (toSource -> moveSource) (toTarget -> moveTarget) matcher = do
  moveId <- getRandom
  pure
    Movement
      { moveSource
      , moveTarget
      , moveDestination = ToLocationMatching matcher
      , moveMeans = Direct
      , moveCancelable = True
      , movePayAdditionalCosts = True
      , moveAfter = []
      , moveAdditionalEnterCosts = Free
      , moveId
      }

moveTowards
  :: (MonadRandom m, Targetable target, Sourceable source, AsId location, IdOf location ~ LocationId)
  => source
  -> target
  -> location
  -> m Movement
moveTowards (toSource -> moveSource) (toTarget -> moveTarget) (asId -> locationId) = do
  moveId <- getRandom
  pure
    Movement
      { moveSource
      , moveTarget
      , moveDestination = ToLocation locationId
      , moveMeans = Towards
      , moveCancelable = True
      , movePayAdditionalCosts = True
      , moveAfter = []
      , moveAdditionalEnterCosts = Free
      , moveId
      }

moveTowardsMatching
  :: (MonadRandom m, Targetable target, Sourceable source)
  => source
  -> target
  -> LocationMatcher
  -> m Movement
moveTowardsMatching (toSource -> moveSource) (toTarget -> moveTarget) matcher = do
  moveId <- getRandom
  pure
    Movement
      { moveSource
      , moveTarget
      , moveDestination = ToLocationMatching matcher
      , moveMeans = Towards
      , moveCancelable = True
      , movePayAdditionalCosts = True
      , moveAfter = []
      , moveAdditionalEnterCosts = Free
      , moveId
      }

moveToLocationMatcher :: Movement -> LocationMatcher
moveToLocationMatcher = destinationToLocationMatcher . moveDestination

destinationToLocationMatcher :: Destination -> LocationMatcher
destinationToLocationMatcher = \case
  ToLocation lid -> LocationWithId lid
  ToLocationMatching matcher -> matcher

$(deriveJSON defaultOptions ''MovementMeans)
$(deriveJSON defaultOptions ''Destination)
$(deriveToJSON defaultOptions ''Movement)

instance FromJSON Movement where
  parseJSON = withObject "Movement" \o -> do
    moveSource <- o .: "moveSource"
    moveTarget <- o .: "moveTarget"
    moveDestination <- o .: "moveDestination"
    moveMeans <- o .: "moveMeans"
    moveCancelable <- o .: "moveCancelable"
    movePayAdditionalCosts <- o .: "movePayAdditionalCosts"
    moveAfter <- o .: "moveAfter"
    moveAdditionalEnterCosts <- o .:? "moveAdditionalEnterCosts" .!= Free
    moveId <- o .:? "moveId" .!= MoveId (fromWords64 6128981282234515924 12039885860129472512)

    pure Movement {..}
