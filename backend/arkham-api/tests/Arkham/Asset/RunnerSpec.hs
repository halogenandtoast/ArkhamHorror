module Arkham.Asset.RunnerSpec (spec) where

import Arkham.Asset.Types (AssetAttrs (..))
import Arkham.Token
import Arkham.Window qualified as Window
import TestImport.New

{- | Removing clues from an asset that never had any must not announce that its
last clue was removed. Without the @assetClues > 0@ guard in Arkham.Asset.Runner,
a single @InvestigatorDiscardAllClues@ broadcast fires this window once per asset
in play -- three full CheckWindows passes each -- which timed out a 4-player
Return to The Depths of Yoth game advancing Journey to the Nexus (issue #5301).
It also let the clue-less Carnevale story assets (Ashleigh Clarke, Jordan Perry,
...) trigger their forced story reveal.
-}
spec :: Spec
spec = describe "Removing clues from an asset" $ do
  context "RemoveAllClues" $ do
    it "announces the last clue removed when the asset had one" . gameTest $ \self -> do
      asset <- withClue self
      assertRunsMessage (lastClueRemoved asset) do
        pushAndRun $ RemoveAllClues (TestSource mempty) (toTarget asset)

    it "stays silent when the asset had no clues" . gameTest $ \self -> do
      asset <- testAsset id self
      assertDoesNotRunMessage (lastClueRemoved asset) do
        pushAndRun $ RemoveAllClues (TestSource mempty) (toTarget asset)

  context "InvestigatorDiscardAllClues" $ do
    it "announces the last clue removed for an owned asset holding one" . gameTest $ \self -> do
      asset <- withClue self
      assertRunsMessage (lastClueRemoved asset) do
        pushAndRun $ InvestigatorDiscardAllClues (TestSource mempty) (toId self)

    it "stays silent for an owned asset holding none" . gameTest $ \self -> do
      asset <- testAsset id self
      assertDoesNotRunMessage (lastClueRemoved asset) do
        pushAndRun $ InvestigatorDiscardAllClues (TestSource mempty) (toId self)
 where
  withClue = testAsset (\attrs -> attrs {assetTokens = setTokens Clue 1 mempty})
  lastClueRemoved asset =
    CheckWindows [Window.mkWhen $ Window.LastClueRemovedFromAsset (toId asset)]
