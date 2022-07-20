module Arkham.Placement where

import Arkham.Prelude

import Arkham.Id

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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
