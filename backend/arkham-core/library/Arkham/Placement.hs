module Arkham.Placement where

import Arkham.Prelude

import Arkham.Id
import Arkham.Target

data Placement
  = AtLocation LocationId
  | AttachedToLocation LocationId
  | InPlayArea InvestigatorId
  | InThreatArea InvestigatorId
  | AttachedToEnemy EnemyId
  | AttachedToAsset AssetId (Maybe Placement)
  | AttachedToAct ActId
  | AttachedToAgenda ActId
  | AttachedToInvestigator InvestigatorId
  | Unplaced
  | TheVoid
  | Pursuit
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

isOutOfPlayPlacement :: Placement -> Bool
isOutOfPlayPlacement = \case
  TheVoid -> True
  Pursuit -> True
  _ -> False

data TreacheryPlacement
  = TreacheryAttachedTo Target
  | TreacheryInHandOf InvestigatorId
  | TreacheryNextToAct
  | TreacheryLimbo
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON , FromJSON)
