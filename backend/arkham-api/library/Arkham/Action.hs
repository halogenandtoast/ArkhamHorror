module Arkham.Action where

import Arkham.Prelude
import Data.Data (dataTypeConstrs, dataTypeOf, fromConstr, showConstr)
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
  | -- | Open extension point for homebrew actions. Do not use directly; each
    -- homebrew campaign owns pattern synonyms over this (see its @Actions.hs@,
    -- e.g. @Arkham.Homebrew.DarkMatter.Actions@). The 'Text' tag is the action
    -- name, so serialization matches a plain enum constructor and needs no
    -- migration.
    HomebrewAction Text
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (Hashable)

-- | Core actions serialize as their bare constructor name (as the derived
-- all-nullary encoding did); a 'HomebrewAction' serializes as its tag, which by
-- construction equals the old constructor name, so saves round-trip. FromJSON
-- resolves core names first, so a promoted homebrew action decodes to it.
instance ToJSON Action where
  toJSON = \case
    HomebrewAction t -> toJSON t
    a -> toJSON (tshow a)

instance FromJSON Action where
  parseJSON = withText "Action" $ \t ->
    pure $ findWithDefault (HomebrewAction t) t coreActionByName

-- | Every non-homebrew action. Replaces @[minBound .. maxBound]@ now that
-- 'Action' carries the open 'HomebrewAction' constructor. For the full set
-- including homebrew, use @Arkham.Homebrew.Defs.allActions@.
coreActions :: [Action]
coreActions =
  [ fromConstr con
  | con <- dataTypeConstrs (dataTypeOf (HomebrewAction ""))
  , showConstr con /= "HomebrewAction"
  ]

coreActionByName :: Map Text Action
coreActionByName = mapFromList [(tshow a, a) | a <- coreActions]

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

instance IsLabel "circle" Action where
  fromLabel = Circle
