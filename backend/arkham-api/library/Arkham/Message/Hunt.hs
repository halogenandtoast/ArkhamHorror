{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.Hunt where

import Arkham.Id
import Arkham.Matcher
import {-# SOURCE #-} Arkham.Message (Message)
import Arkham.Prelude
import Arkham.Target
import Data.Aeson.TH

-- | Messages dealing with enemy hunting, movement, and elusive flight.
data HuntMessage
  = EnemyMove_ EnemyId LocationId
  | HuntersMove_
  | HunterMove_ EnemyId
  | PatrolMove_ EnemyId LocationMatcher
  | MoveToward_ Target LocationMatcher
  | MoveUntil_ LocationId Target
  | WillMoveEnemy_ EnemyId Message
  | HandleElusive_ EnemyId
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''HuntMessage)
