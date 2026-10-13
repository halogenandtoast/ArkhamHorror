module Arkham.Event.Events.TimeWarp2Spec (spec) where

import Arkham.Action qualified as Action
import Arkham.Classes.HasGame (getGame)
import Arkham.Event.Cards qualified as Events
import Arkham.Window qualified as Window
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import TestImport.New

-- Mutate state inside the action WITHOUT going through the action pipeline,
-- which would FinishAction and clear the revert bookkeeping under test.
gainResource :: Investigator -> TestAppT ()
gainResource self = run $ TakeResources (toId self) 1 (toSource self) False

-- Time Warp (2)'s fast window: PerformAction #after ... AnyAction.
playAfterAction :: Investigator -> Card -> TestAppT ()
playAfterAction self card =
  run
    $ InitiatePlayCardWithWindows
      (toId self)
      card
      Nothing
      NoPayment
      [Window.mkAfter $ Window.PerformAction (toId self) Action.Investigate]
      True

spec :: Spec
spec = describe "Time Warp (2)" do
  -- The card's whole effect is `push UndoAction`, which folds gameActionDiff.
  it "undoes the effects of the action it was played after" . gameTest $ \self -> do
    withProp @"resources" 0 self
    timeWarp <- genCard Events.timeWarp2
    self `addToHand` timeWarp
    duringTurn self do
      run BeginAction
      gainResource self
      gainResource self
      self.resources `shouldReturn` 2
      playAfterAction self timeWarp
      self.resources `shouldReturn` 0

  {- The invariant that decides whether dropping arkham_steps.action_diff is
  safe: the revert data Time Warp consumes travels in the Game itself, i.e. in
  arkham_games.current_data, and nowhere else. This whole suite runs with no
  arkham_steps table in existence, so if the card works here it cannot be
  reading the step column -- but assert the field explicitly so a future change
  that moves the data out of the Game fails loudly here rather than silently
  breaking the card in production.
  -}
  it "carries its revert data in the Game, not in a step row" . gameTest $ \self -> do
    run BeginAction
    gainResource self
    g <- getGame
    null (gameActionDiff g) `shouldBe` False
    -- present in the serialized game, which is what current_data stores
    case Aeson.toJSON g of
      Aeson.Object o -> isJust (KeyMap.lookup "gameActionDiff" o) `shouldBe` True
      _ -> error "a serialized Game should be a JSON object"

  -- Reconstructing the action start after a mid-action load uses only the
  -- loaded Game (handleActionDiff, Game/Runner.hs). Proves Time Warp survives
  -- the save/load boundary with no step row involved.
  it "still undoes the action after a mid-action save/load round-trip" . gameTest $ \self -> do
    withProp @"resources" 0 self
    timeWarp <- genCard Events.timeWarp2
    self `addToHand` timeWarp
    duringTurn self do
      run BeginAction
      gainResource self
      g1 <- getGame
      loaded <- case Aeson.fromJSON (Aeson.toJSON g1) of
        Aeson.Error e -> error $ "game failed to round-trip: " <> e
        Aeson.Success (loaded :: Game) -> pure loaded
      testApp <- get
      atomicWriteIORef (game testApp) loaded
      gainResource self
      self.resources `shouldReturn` 2
      playAfterAction self timeWarp
      self.resources `shouldReturn` 0
