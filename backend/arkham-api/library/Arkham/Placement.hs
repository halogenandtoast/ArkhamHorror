{-# LANGUAGE TemplateHaskell #-}

module Arkham.Placement (
  Placement (..),
  IsPlacement (..),
  placementToAttached,
  isDirectlyAtLocation,
  betweenLocations,
  atLocations,
  isOutOfPlayPlacement,
  isInPlayPlacement,
  isHiddenPlacement,
  isInPlayArea,
  treacheryPlacementToPlacement,
  _AtLocation,
  _FacedownInThreatArea,
  _OutOfPlay,
) where

import Arkham.Card
import Arkham.Id
import Arkham.Location.Grid
import Arkham.Prelude
import Arkham.Target
import Arkham.Zone
import Data.Aeson.TH
import GHC.Records

data Placement
  = AtLocation LocationId
  | {- | At *every* one of these locations at once (Circus Ex Mortis, "Sylvester
    Blake is considered to be at each The Big Top location"). Unlike
    'BetweenLocations' — which is on the connection and so at neither end — this
    is at all of them, so 'EnemyAt', Massive engagement and @Here@ all see it.
    -}
    AtLocations (NonEmpty LocationId)
  | AttachedToLocation LocationId
  | {- | Sits on the *connection* between two locations rather than on either of
    them (Circus Ex Mortis, "Broken Couplings"). Build it with
    'betweenLocations' so the pair is order-normalized and two placements of
    the same connection compare equal.
    -}
    BetweenLocations LocationId LocationId
  | InPlayArea InvestigatorId
  | InThreatArea InvestigatorId
  | {- | An encounter card sitting *face down* in an investigator's threat area
    (Dark Matter, "Lost Quantum"). The entity exists but its revelation has
    not resolved; it resolves when the card is later "drawn" from the threat
    area. Face down, so it is not in play.
    -}
    FacedownInThreatArea InvestigatorId
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
  | NextToAct
  | NextToScenarioReference
  | InVehicle AssetId
  | AttachedToInvestigator InvestigatorId
  | AsSwarm {swarmHost :: EnemyId, swarmCard :: Card}
  | Unplaced
  | Limbo
  | Global
  | OutOfPlay OutOfPlayZone
  | Near Target
  | InTheShadows
  | OutOfGame Placement
  | InPosition Pos
  | {- | The card occupies a grid cell of its own, named by that label in the scenario's
    layout, rather than sitting at a location. Generalised from the enemy-only
    @enemyAsSelfLocation@: an enemy that is its own location (Sylvester Blake across the
    Big Top), and Red Sunrise's Path Forward stories, which sit beside a row of
    locations but at no location.
    -}
    AsSelfLocation Text
  deriving stock (Show, Eq, Ord, Data, Generic)

instance HasField "attachedTo" Placement (Maybe Target) where
  getField = placementToAttached

instance HasField "outOfGame" Placement Bool where
  getField = \case
    OutOfGame _ -> True
    _ -> False

instance HasField "isAttached" Placement Bool where
  getField = isJust . placementToAttached

instance HasField "isInPlay" Placement Bool where
  getField = isInPlayPlacement

instance HasField "isInVictory" Placement Bool where
  getField = \case
    OutOfPlay VictoryDisplayZone -> True
    _ -> False

instance HasField "isSwarm" Placement Bool where
  getField = \case
    AsSwarm {} -> True
    _ -> False

instance HasField "inThreatAreaOf" Placement (Maybe InvestigatorId) where
  getField = \case
    InThreatArea iid -> Just iid
    _ -> Nothing

placementToAttached :: Placement -> Maybe Target
placementToAttached = \case
  AttachedToLocation lid -> Just $ LocationTarget lid
  BetweenLocations _ _ -> Nothing
  AttachedToEnemy eid -> Just $ EnemyTarget eid
  AttachedToTreachery tid -> Just $ TreacheryTarget tid
  Near _ -> Nothing
  AtLocation _ -> Nothing
  AtLocations _ -> Nothing
  InPlayArea _ -> Nothing
  InVehicle _ -> Nothing
  InThreatArea _ -> Nothing
  FacedownInThreatArea _ -> Nothing
  AttachedToAsset aid _ -> Just $ AssetTarget aid
  AttachedToAct aid -> Just $ ActTarget aid
  AttachedToAgenda aid -> Just $ AgendaTarget aid
  NextToAgenda -> Nothing
  NextToAct -> Nothing
  NextToScenarioReference -> Nothing
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
  InTheShadows -> Nothing
  OutOfGame _ -> Nothing
  InPosition _ -> Nothing
  AsSelfLocation _ -> Nothing

isOutOfPlayPlacement :: Placement -> Bool
isOutOfPlayPlacement = not . isInPlayPlacement

isInPlayPlacement :: Placement -> Bool
isInPlayPlacement = \case
  AtLocation {} -> True
  AtLocations {} -> True
  AttachedToLocation {} -> True
  BetweenLocations {} -> True
  InPlayArea {} -> True
  InVehicle {} -> True
  InThreatArea {} -> True
  FacedownInThreatArea {} -> False
  StillInHand {} -> False
  StillInDiscard {} -> False
  StillInEncounterDiscard -> False
  AttachedToEnemy {} -> True
  AttachedToTreachery {} -> True
  AttachedToAsset {} -> True
  AttachedToAct {} -> True
  AttachedToAgenda {} -> True
  NextToAgenda {} -> True -- is it in play, idk
  NextToAct {} -> True -- is it in play, idk
  NextToScenarioReference {} -> True
  AttachedToInvestigator {} -> True
  AsSwarm {} -> True
  Unplaced {} -> False
  Limbo {} -> False
  Global {} -> True
  OutOfPlay {} -> False
  HiddenInHand _ -> False
  OnTopOfDeck _ -> False
  Near _ -> True
  InTheShadows -> True
  OutOfGame _ -> False
  InPosition _ -> True
  AsSelfLocation _ -> True

isHiddenPlacement :: Placement -> Bool
isHiddenPlacement = \case
  HiddenInHand _ -> True
  FacedownInThreatArea _ -> True
  _ -> False

{- | Whether this placement sits *directly* on the given location, as opposed to
reaching it transitively through an enemy, asset, treachery, vehicle or
investigator standing there. Those attachments are their host's
responsibility when the host leaves play, see #5426.
-}
isDirectlyAtLocation :: LocationId -> Placement -> Bool
isDirectlyAtLocation lid = \case
  AtLocation lid' -> lid' == lid
  AtLocations lids -> lid `elem` lids
  AttachedToLocation lid' -> lid' == lid
  BetweenLocations a b -> a == lid || b == lid
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

mconcat
  [ deriveToJSON defaultOptions ''Placement
  , makePrisms ''Placement
  ]

-- | Order-normalized so the same connection is always the same placement.
betweenLocations :: LocationId -> LocationId -> Placement
betweenLocations a b = if a <= b then BetweenLocations a b else BetweenLocations b a

-- | At each of these locations at once; a single location stays 'AtLocation'.
atLocations :: [LocationId] -> Placement
atLocations = \case
  [] -> Unplaced
  [lid] -> AtLocation lid
  (lid : lids) -> AtLocations (lid :| lids)

class IsPlacement a where
  toPlacement :: a -> Placement

instance IsPlacement Placement where
  toPlacement = id

instance IsPlacement OutOfPlayZone where
  toPlacement = OutOfPlay

instance IsPlacement LocationId where
  toPlacement = AtLocation

instance IsPlacement AssetId where
  toPlacement = (`AttachedToAsset` Nothing)
