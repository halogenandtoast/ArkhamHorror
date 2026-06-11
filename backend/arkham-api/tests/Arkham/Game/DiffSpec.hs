module Arkham.Game.DiffSpec (spec) where

import Arkham.Classes.HasGame (getGame)
import Arkham.Game.Diff (patchWithRecovery)
import Data.Aeson qualified as Aeson
import TestImport.New

-- gain a resource without going through the action pipeline (which would
-- FinishAction and clear the undo bookkeeping we are testing)
gainResource :: Investigator -> TestAppT ()
gainResource self = run $ TakeResources (toId self) 1 (toSource self) False

spec :: Spec
spec = describe "action undo bookkeeping" $ do
  it "keeps a single revert patch that restores the action start" . gameTest $ \self -> do
    g0 <- getGame
    run BeginAction
    gainResource self
    gainResource self
    g1 <- getGame
    p <- case gameActionDiff g1 of
      [p] -> pure p
      ps -> error $ "expected exactly one revert patch, got " <> show (length ps)
    case patchWithRecovery g1 p of
      Aeson.Error e -> error $ "revert patch failed to apply: " <> e
      Aeson.Success reverted ->
        Aeson.toJSON (reverted {gameActionDiff = []})
          `shouldBe` Aeson.toJSON (g0 {gameActionDiff = []})

  it "UndoAction reverts the game to the BeginAction state" . gameTest $ \self -> do
    self.resources `shouldReturn` 0
    run BeginAction
    gainResource self
    gainResource self
    self.resources `shouldReturn` 2
    run UndoAction
    self.resources `shouldReturn` 0

  it "UndoAction still reverts after a mid-action save/load round-trip" . gameTest $ \self -> do
    g0 <- getGame
    run BeginAction
    gainResource self
    -- simulate the save + load the server performs between answers: the
    -- runtime snapshot is dropped and must be rebuilt from the saved patches
    g1 <- getGame
    loaded <- case Aeson.fromJSON (Aeson.toJSON g1) of
      Aeson.Error e -> error $ "game failed to round-trip: " <> e
      Aeson.Success (loaded :: Game) -> pure loaded
    testApp <- get
    atomicWriteIORef (game testApp) loaded
    gainResource self
    self.resources `shouldReturn` 2
    run UndoAction
    g2 <- getGame
    Aeson.toJSON (g2 {gameActionDiff = []})
      `shouldBe` Aeson.toJSON (g0 {gameActionDiff = []})
    self.resources `shouldReturn` 0
