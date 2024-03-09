module Arkham.Helpers.Placement where

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Message
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Window (mkAfter)
import Arkham.Window qualified as Window

placedInThreatArea :: HasGame m => Placement -> m (Maybe InvestigatorId)
placedInThreatArea = \case
  AtLocation _ -> pure Nothing
  AttachedToLocation _ -> pure Nothing
  InPlayArea _ -> pure Nothing
  InThreatArea iid -> pure $ Just iid
  StillInHand _ -> pure Nothing
  StillInDiscard _ -> pure Nothing
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

checkEntersThreatArea :: (HasGame m, HasQueue Message m, IsCard a) => a -> Placement -> m ()
checkEntersThreatArea a p =
  placedInThreatArea p >>= \case
    Just iid -> do
      pushM $ checkWindows [mkAfter $ Window.EntersThreatArea iid (toCard a)]
    _ -> pure ()
