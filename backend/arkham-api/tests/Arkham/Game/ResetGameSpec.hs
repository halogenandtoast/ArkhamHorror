module Arkham.Game.ResetGameSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Slot
import Arkham.Matcher
import Arkham.Projection
import TestImport.Lifted

spec :: Spec
spec = describe "ResetGame" $ do
  -- #5593: investigatorSlots is not part of gameEntities, so the asset wipe below leaves
  -- dangling AssetIds behind. `ForInvestigators [] ResetGame` normally rebuilds the slot
  -- map right after, but skipInvestigatorSetup (Hemlock Vale's afterPrelude) skips it, and
  -- the next InvestigatorClearUnusedAssetSlots then died on MissingEntity.
  it "drops slot occupants whose asset it just removed" $ gameTest $ \self -> do
    putCardIntoPlay self Assets.machete
    machete <- selectJust $ assetIs Assets.machete
    occupants <- slotOccupants self
    occupants `shouldContain` [machete]

    run $ ForTarget GameTarget ResetGame
    occupants' <- slotOccupants self
    occupants' `shouldSatisfy` notElem machete

  it "drops slots granted by an asset it just removed" $ gameTest $ \self -> do
    putCardIntoPlay self Assets.bandolier
    bandolier <- selectJust $ assetIs Assets.bandolier
    handSlots <- findWithDefault [] HandSlot <$> field InvestigatorSlots (toId self)
    handSlots `shouldSatisfy` any (isSlotSource bandolier)

    run $ ForTarget GameTarget ResetGame
    handSlots' <- findWithDefault [] HandSlot <$> field InvestigatorSlots (toId self)
    handSlots' `shouldSatisfy` (not . any (isSlotSource bandolier))
 where
  slotOccupants self = do
    slots <- field InvestigatorSlots (toId self)
    pure $ concatMap slotItems $ concat $ toList slots
