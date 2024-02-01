module Arkham.Action where

import Arkham.Prelude
import GHC.OverloadedLabels

data ActionType
  = EnemyActionType
  | LocationActionType
  | AssetActionType
  | TreacheryActionType
  | ActActionType
  | AgendaActionType
  | InvestigatorActionType
  deriving stock (Bounded, Enum, Show)

newtype TakenAction = TakenAction {unTakenAction :: Action}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Action
  = Activate
  | Draw
  | Engage
  | Evade
  | Fight
  | Investigate
  | Move
  | Parley
  | Play
  | Resign
  | Resource
  | Explore
  | Circle
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic, Data)
  deriving anyclass (ToJSON, FromJSON, Hashable, NoThunks)

instance IsLabel "activate" Action where
  fromLabel = Activate

instance IsLabel "investigate" Action where
  fromLabel = Investigate

instance IsLabel "move" Action where
  fromLabel = Move

instance IsLabel "evade" Action where
  fromLabel = Evade

instance IsLabel "fight" Action where
  fromLabel = Fight

instance IsLabel "resign" Action where
  fromLabel = Resign

instance IsLabel "parley" Action where
  fromLabel = Parley

instance IsLabel "play" Action where
  fromLabel = Play

instance IsLabel "engage" Action where
  fromLabel = Engage

instance IsLabel "resource" Action where
  fromLabel = Resource

instance IsLabel "draw" Action where
  fromLabel = Draw

instance IsLabel "explore" Action where
  fromLabel = Explore
