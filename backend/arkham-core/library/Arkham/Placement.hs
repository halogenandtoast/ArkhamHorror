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
  | AttachedToAgenda AgendaId
  | AttachedToInvestigator InvestigatorId
  | Unplaced
  | TheVoid
  | Pursuit
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

placementToAttached :: Placement -> Maybe Target
placementToAttached = \case
  AttachedToLocation lid -> Just $ LocationTarget lid
  AttachedToEnemy eid -> Just $ EnemyTarget eid
  AtLocation _ -> Nothing
  InPlayArea _ -> Nothing
  InThreatArea _ -> Nothing
  AttachedToAsset _ (Just p) -> placementToAttached p
  AttachedToAsset aid Nothing -> Just $ AssetTarget aid
  AttachedToAct aid -> Just $ ActTarget aid
  AttachedToAgenda aid -> Just $ AgendaTarget aid
  AttachedToInvestigator iid -> Just $ InvestigatorTarget iid
  Unplaced -> Nothing
  TheVoid -> Nothing
  Pursuit -> Nothing

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
