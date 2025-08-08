{-# LANGUAGE TemplateHaskell #-}

module Arkham.Placement (
  Placement (..),
  placementToAttached,
  isOutOfPlayPlacement,
  isOutOfPlayZonePlacement,
  isInPlayPlacement,
  isHiddenPlacement,
  isInPlayArea,
  treacheryPlacementToPlacement,
) where

import Arkham.Card
import Arkham.Id
import Arkham.Prelude
import Arkham.Target
import Arkham.Zone
import Data.Aeson.TH
import GHC.Records

data Placement
  = AtLocation LocationId
  | AttachedToLocation LocationId
  | InPlayArea InvestigatorId
  | InThreatArea InvestigatorId
  | ActuallyLocation LocationId
  | StillInHand InvestigatorId
  | HiddenInHand InvestigatorId
  | OnTopOfDeck InvestigatorId
  | StillInDiscard InvestigatorId
  | StillInEncounterDiscard
  | AttachedToEnemy EnemyId
  | AttachedToTreachery TreacheryId -- Not used yet?
  | AttachedToAsset AssetId (Maybe Placement) -- Maybe Placement for Dr. Elli Horowitz
  | AttachedToAct ActId
  | AttachedToAgenda AgendaId
  | NextToAgenda
  | InVehicle AssetId
  | AttachedToInvestigator InvestigatorId
  | AsSwarm {swarmHost :: EnemyId, swarmCard :: Card}
  | Unplaced
  | Limbo
  | Global
  | OutOfPlay OutOfPlayZone
  | Near Target
  deriving stock (Show, Eq, Ord, Data, Generic)

instance HasField "attachedTo" Placement (Maybe Target) where
  getField = placementToAttached

instance HasField "isAttached" Placement Bool where
  getField = isJust . placementToAttached

instance HasField "isInPlay" Placement Bool where
  getField = isInPlayPlacement

instance HasField "isInVictory" Placement Bool where
  getField = \case
    OutOfPlay VictoryDisplayZone -> True
    _ -> False

instance HasField "inThreatAreaOf" Placement (Maybe InvestigatorId) where
  getField = \case
    InThreatArea iid -> Just iid
    _ -> Nothing

placementToAttached :: Placement -> Maybe Target
placementToAttached = \case
  AttachedToLocation lid -> Just $ LocationTarget lid
  AttachedToEnemy eid -> Just $ EnemyTarget eid
  AttachedToTreachery tid -> Just $ TreacheryTarget tid
  Near _ -> Nothing
  AtLocation _ -> Nothing
  ActuallyLocation _ -> Nothing
  InPlayArea _ -> Nothing
  InVehicle _ -> Nothing
  InThreatArea _ -> Nothing
  AttachedToAsset aid _ -> Just $ AssetTarget aid
  AttachedToAct aid -> Just $ ActTarget aid
  AttachedToAgenda aid -> Just $ AgendaTarget aid
  NextToAgenda -> Nothing
  AttachedToInvestigator iid -> Just $ InvestigatorTarget iid
  Unplaced -> Nothing
  Global -> Nothing
  Limbo -> Nothing
  OutOfPlay _ -> Nothing
  StillInHand _ -> Nothing
  StillInDiscard _ -> Nothing
  StillInEncounterDiscard -> Nothing
  AsSwarm _ _ -> Nothing
  HiddenInHand _ -> Nothing
  OnTopOfDeck _ -> Nothing

isOutOfPlayZonePlacement :: Placement -> Bool
isOutOfPlayZonePlacement = \case
  OutOfPlay _ -> True
  _ -> False

isOutOfPlayPlacement :: Placement -> Bool
isOutOfPlayPlacement = not . isInPlayPlacement

isInPlayPlacement :: Placement -> Bool
isInPlayPlacement = \case
  AtLocation {} -> True
  ActuallyLocation {} -> True
  AttachedToLocation {} -> True
  InPlayArea {} -> True
  InVehicle {} -> True
  InThreatArea {} -> True
  StillInHand {} -> False
  StillInDiscard {} -> False
  StillInEncounterDiscard -> False
  AttachedToEnemy {} -> True
  AttachedToTreachery {} -> True
  AttachedToAsset {} -> True
  AttachedToAct {} -> True
  AttachedToAgenda {} -> True
  NextToAgenda {} -> True -- is it in play, idk
  AttachedToInvestigator {} -> True
  AsSwarm {} -> True
  Unplaced {} -> False
  Limbo {} -> False
  Global {} -> True
  OutOfPlay {} -> False
  HiddenInHand _ -> False
  OnTopOfDeck _ -> False
  Near _ -> True

isHiddenPlacement :: Placement -> Bool
isHiddenPlacement = \case
  HiddenInHand _ -> True
  _ -> False

isInPlayArea :: Placement -> Bool
isInPlayArea = \case
  InPlayArea _ -> True
  AttachedToAsset _ (Just (InPlayArea _)) -> True
  _ -> False

data TreacheryPlacement
  = TreacheryAttachedTo Target
  | TreacheryInHandOf InvestigatorId
  | TreacheryNextToAgenda
  | TreacheryLimbo
  | TreacheryTopOfDeck InvestigatorId
  deriving stock (Show, Eq, Data)

treacheryPlacementToPlacement :: TreacheryPlacement -> Placement
treacheryPlacementToPlacement = \case
  TreacheryAttachedTo target -> case target of
    LocationTarget lid -> AttachedToLocation lid
    EnemyTarget eid -> AttachedToEnemy eid
    AssetTarget aid -> AttachedToAsset aid Nothing
    ActTarget aid -> AttachedToAct aid
    AgendaTarget aid -> AttachedToAgenda aid
    InvestigatorTarget iid -> AttachedToInvestigator iid
    _ -> error $ "Unhandled attached to conversion: " <> show target
  TreacheryNextToAgenda -> NextToAgenda
  TreacheryInHandOf iid -> HiddenInHand iid
  TreacheryLimbo -> Limbo
  TreacheryTopOfDeck iid -> OnTopOfDeck iid

$(deriveJSON defaultOptions ''TreacheryPlacement)

instance FromJSON Placement where
  parseJSON o = genericParseJSON defaultOptions o <|> (treacheryPlacementToPlacement <$> parseJSON o)

$(deriveToJSON defaultOptions ''Placement)
