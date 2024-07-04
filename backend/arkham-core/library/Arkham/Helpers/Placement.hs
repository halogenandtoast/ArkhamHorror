module Arkham.Helpers.Placement (module X, module Arkham.Helpers.Placement) where

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Message
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Placement as X
import Arkham.Prelude
import Arkham.Projection
import Arkham.Target
import Arkham.Window qualified as Window

placedInThreatArea :: HasGame m => Placement -> m (Maybe InvestigatorId)
placedInThreatArea = \case
  AtLocation _ -> pure Nothing
  AttachedToLocation _ -> pure Nothing
  InPlayArea _ -> pure Nothing
  InThreatArea iid -> pure $ Just iid
  StillInHand _ -> pure Nothing
  StillInDiscard _ -> pure Nothing
  StillInEncounterDiscard -> pure Nothing
  AttachedToEnemy eid -> fieldMapM EnemyPlacement placedInThreatArea eid
  AttachedToAsset aid _ -> fieldMapM AssetPlacement placedInThreatArea aid
  AttachedToAct _ -> pure Nothing
  AttachedToAgenda _ -> pure Nothing
  AttachedToInvestigator _ -> pure Nothing
  AsSwarm {swarmHost} -> fieldMapM EnemyPlacement placedInThreatArea swarmHost
  Unplaced -> pure Nothing
  Limbo -> pure Nothing
  Global -> pure Nothing
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
