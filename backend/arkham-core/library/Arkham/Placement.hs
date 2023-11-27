{-# LANGUAGE TemplateHaskell #-}

module Arkham.Placement where

import Arkham.Prelude

import Arkham.Card
import Arkham.Id
import Arkham.Target
import Arkham.Zone
import Data.Aeson.TH

data Placement
  = AtLocation LocationId
  | AttachedToLocation LocationId
  | InPlayArea InvestigatorId
  | InThreatArea InvestigatorId
  | StillInHand InvestigatorId
  | StillInDiscard InvestigatorId
  | AttachedToEnemy EnemyId
  | AttachedToAsset AssetId (Maybe Placement) -- Maybe Placement for Dr. Elli Horowitz
  | AttachedToAct ActId
  | AttachedToAgenda AgendaId
  | AttachedToInvestigator InvestigatorId
  | AsSwarm {swarmHost :: EnemyId, swarmCard :: Card}
  | Unplaced
  | Limbo
  | Global
  | OutOfPlay OutOfPlayZone
  deriving stock (Show, Eq, Ord, Data)

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
  Global -> Nothing
  Limbo -> Nothing
  OutOfPlay _ -> Nothing
  StillInHand _ -> Nothing
  StillInDiscard _ -> Nothing
  AsSwarm _ _ -> Nothing

isOutOfPlayPlacement :: Placement -> Bool
isOutOfPlayPlacement = \case
  OutOfPlay _ -> True
  _ -> False

isInPlayPlacement :: Placement -> Bool
isInPlayPlacement = \case
  AtLocation {} -> True
  AttachedToLocation {} -> True
  InPlayArea {} -> True
  InThreatArea {} -> True
  StillInHand {} -> False
  StillInDiscard {} -> False
  AttachedToEnemy {} -> True
  AttachedToAsset {} -> True
  AttachedToAct {} -> True
  AttachedToAgenda {} -> True
  AttachedToInvestigator {} -> True
  AsSwarm {} -> True
  Unplaced {} -> False
  Limbo {} -> False
  Global {} -> True
  OutOfPlay {} -> False

data TreacheryPlacement
  = TreacheryAttachedTo Target
  | TreacheryInHandOf InvestigatorId
  | TreacheryNextToAgenda
  | TreacheryLimbo
  deriving stock (Show, Eq, Data)

$(deriveJSON defaultOptions ''Placement)
$(deriveJSON defaultOptions ''TreacheryPlacement)
