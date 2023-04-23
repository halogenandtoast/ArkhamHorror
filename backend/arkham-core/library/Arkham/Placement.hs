module Arkham.Placement where

import Arkham.Prelude

import Arkham.Id
import Arkham.Target
import Arkham.Scenario.Zone

data Placement
  = AtLocation LocationId
  | AttachedToLocation LocationId
  | InPlayArea InvestigatorId
  | InThreatArea InvestigatorId
  | StillInHand InvestigatorId
  | AttachedToEnemy EnemyId
  | AttachedToAsset AssetId (Maybe Placement) -- Maybe Placement for Dr. Elli Horowitz
  | AttachedToAct ActId
  | AttachedToAgenda AgendaId
  | AttachedToInvestigator InvestigatorId
  | Unplaced
  | Limbo
  | ScenarioZone ScenarioZone
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
  Limbo -> Nothing
  ScenarioZone _ -> Nothing
  StillInHand _ -> Nothing

isOutOfPlayPlacement :: Placement -> Bool
isOutOfPlayPlacement = \case
  ScenarioZone sZone -> case sZone of
    VoidZone -> True
    PursuitZone -> True
  _ -> False

data TreacheryPlacement
  = TreacheryAttachedTo Target
  | TreacheryInHandOf InvestigatorId
  | TreacheryNextToAct
  | TreacheryLimbo
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON , FromJSON)
