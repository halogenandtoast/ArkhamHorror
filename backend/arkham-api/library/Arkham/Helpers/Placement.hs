module Arkham.Helpers.Placement (module X, module Arkham.Helpers.Placement) where

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Message
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Placement as X
import Arkham.Prelude
import Arkham.Projection
import Arkham.Target
import Arkham.Treachery.Types (Field (..))
import Arkham.Window qualified as Window

placedInThreatArea :: (HasCallStack, HasGame m) => Placement -> m (Maybe InvestigatorId)
placedInThreatArea = \case
  AtLocation _ -> pure Nothing
  AttachedToLocation _ -> pure Nothing
  InPlayArea _ -> pure Nothing
  InVehicle _ -> pure Nothing
  InThreatArea iid -> pure $ Just iid
  StillInHand _ -> pure Nothing
  StillInDiscard _ -> pure Nothing
  StillInEncounterDiscard -> pure Nothing
  AttachedToEnemy eid -> fieldMapM EnemyPlacement placedInThreatArea eid
  AttachedToTreachery tid -> fieldMapM TreacheryPlacement placedInThreatArea tid
  AttachedToAsset aid _ -> fieldMapM AssetPlacement placedInThreatArea aid
  AttachedToAct _ -> pure Nothing
  AttachedToAgenda _ -> pure Nothing
  AttachedToInvestigator _ -> pure Nothing
  AsSwarm {swarmHost} -> fieldMapM EnemyPlacement placedInThreatArea swarmHost
  Unplaced -> pure Nothing
  Limbo -> pure Nothing
  Global -> pure Nothing
  Near _ -> pure Nothing
  OutOfPlay _ -> pure Nothing
  HiddenInHand _ -> pure Nothing
  OnTopOfDeck _ -> pure Nothing
  NextToAgenda -> pure Nothing

checkEntersThreatArea :: (HasGame m, HasQueue Message m, IsCard a) => a -> Placement -> m ()
checkEntersThreatArea a p =
  placedInThreatArea p >>= traverse_ \iid -> do
    pushM $ checkAfter $ Window.EntersThreatArea iid (toCard a)

attachTo :: (HasCallStack, Targetable t, Show t) => t -> Placement
attachTo t = case toTarget t of
  LocationTarget lid -> AttachedToLocation lid
  EnemyTarget eid -> AttachedToEnemy eid
  AssetTarget aid -> AttachedToAsset aid Nothing
  ActTarget aid -> AttachedToAct aid
  AgendaTarget aid -> AttachedToAgenda aid
  InvestigatorTarget iid -> AttachedToInvestigator iid
  _ -> error $ "cannot attach to target: " <> show t

onSameLocation :: (HasCallStack, HasGame m) => InvestigatorId -> Placement -> m Bool
onSameLocation iid = \case
  AttachedToLocation lid -> fieldMap InvestigatorLocation (== Just lid) iid
  AtLocation lid -> fieldMap InvestigatorLocation (== Just lid) iid
  InVehicle aid -> do
    field AssetLocation aid >>= \case
      Nothing -> pure False
      Just lid -> fieldMap InvestigatorLocation (== Just lid) iid
  InPlayArea iid' ->
    if iid == iid'
      then pure True
      else do
        l1 <- join <$> fieldMay InvestigatorLocation iid
        l2 <- join <$> fieldMay InvestigatorLocation iid'
        pure $ isJust l1 && l1 == l2
  InThreatArea iid' ->
    if iid == iid'
      then pure True
      else do
        l1 <- join <$> fieldMay InvestigatorLocation iid
        l2 <- join <$> fieldMay InvestigatorLocation iid'
        pure $ isJust l1 && l1 == l2
  AttachedToEnemy eid -> do
    fieldMay EnemyLocation eid >>= \case
      Nothing -> pure False
      Just p -> (== p) <$> field InvestigatorLocation iid
  AttachedToTreachery tid ->
    liftA2 (==) (field TreacheryLocation tid) (field InvestigatorLocation iid)
  AttachedToAsset aid _ -> do
    placement' <- field AssetPlacement aid
    onSameLocation iid placement'

  -- fieldMay AssetPlacement aid >>= \case
  --   Nothing -> pure False
  --   Just placement' -> onSameLocation iid placement'
  AttachedToAct _ -> pure False
  AttachedToAgenda _ -> pure False
  AttachedToInvestigator iid' ->
    liftA2
      (==)
      (field InvestigatorLocation iid')
      (field InvestigatorLocation iid)
  AsSwarm eid _ -> fieldMay EnemyPlacement eid >>= \case
    Nothing -> pure False
    Just placement' -> onSameLocation iid placement'
  Unplaced -> pure False
  Global -> pure True
  Limbo -> pure False
  OutOfPlay _ -> pure False
  StillInHand _ -> pure False
  StillInDiscard _ -> pure False
  StillInEncounterDiscard -> pure False
  HiddenInHand _ -> pure False
  OnTopOfDeck _ -> pure False
  NextToAgenda -> pure False
  Near _ -> pure False
