module Arkham.Game.PendingGameOptionsSpec (spec) where

import Api.Arkham.Helpers (GameApp (..), runGameApp)
import Arkham.Campaign.Option (CampaignOption (UseSwarmPlaceholders))
import Arkham.Classes.HasGame (getGame)
import Arkham.Classes.HasQueue (newQueue, peekQueue, push)
import Arkham.Difficulty (Difficulty (Standard))
import Arkham.Game (addPlayer, gameGameState, newCampaign)
import Arkham.Game.State (GameState (IsActive, IsPending))
import Arkham.Id (PlayerId (..))
import Arkham.Message (Message (HandleOption, StartCampaign))
import Arkham.Prelude
import Data.UUID qualified as UUID
import System.Random (mkStdGen)
import Test.Hspec

{- | Regression for #5418. A WithFriends game is created with only ONE seat filled
(@repeatCount = 1@ in 'postApiV1ArkhamGamesR'), so it stays 'IsPending' while the
chosen campaign options are pushed as 'HandleOption' messages. 'runMessages'
refuses to consume anything while pending, so they are persisted in the step
queue and must survive until the lobby fills. 'addPlayer' used to overwrite the
whole queue with @[StartCampaign]@ on the IsActive transition, silently throwing
them away -- The Dream-Eaters ran with no UseSwarmPlaceholders and no variant.
-}
spec :: Spec
spec = describe "pending multiplayer lobby" do
  let
    p1 = PlayerId (UUID.fromWords 0 0 0 1)
    p2 = PlayerId (UUID.fromWords 0 0 0 2)
    -- The Dream-Eaters, two seats.
    newApp = do
      gameRef <- newIORef (newCampaign "06" Nothing 0 2 Standard False)
      queueRef <- newQueue []
      genRef <- newIORef (mkStdGen 0)
      pure $ GameApp gameRef queueRef genRef (pure . const ()) Nothing

  it "keeps the queued campaign options when the last player joins" do
    app <- newApp

    -- Game creation: one seat, then the options the creator picked.
    (state0, queue0) <- runGameApp app do
      addPlayer p1
      push (HandleOption UseSwarmPlaceholders)
      (,) <$> (gameGameState <$> getGame) <*> peekQueue
    state0 `shouldBe` IsPending [p1]
    queue0 `shouldBe` [HandleOption UseSwarmPlaceholders]

    -- The friend joins: the lobby activates, and StartCampaign must be appended
    -- so the options are still folded into the campaign log ahead of it.
    (state1, queue1) <- runGameApp app do
      addPlayer p2
      (,) <$> (gameGameState <$> getGame) <*> peekQueue
    state1 `shouldBe` IsActive
    queue1 `shouldBe` [HandleOption UseSwarmPlaceholders, StartCampaign]
