module Arkham.Treachery.Cards.PursuedSpec (spec) where

import Arkham.Classes.HasGame (getGame)
import Arkham.Movement (move)
import Arkham.Treachery.Cards qualified as Cards
import TestImport.New

-- The buggy behaviour left a forced-ability question pending rather than
-- assigning horror outright, so asserting on horror alone would pass either
-- way. Assert the window never opened.
assertNoPendingQuestion :: TestAppT ()
assertNoPendingQuestion = do
  questionMap <- gameQuestion <$> getGame
  when (notNull questionMap)
    $ expectationFailure "expected no pending question, but a window was opened"

spec :: Spec
spec = describe "Pursued" $ do
  it "triggers when an enemy moves into your location" . gameTest $ \self -> do
    (here, there) <- testConnectedLocations id id
    self `moveTo` here
    enemy <- testEnemy
    enemy `spawnAt` there
    loadDeck self [Cards.pursued]
    drawCards self 1
    run . Move =<< move (toSource enemy) (toTarget enemy) (toId here)
    click "trigger Pursued"
    click "assign the horror to the investigator"
    self.horror `shouldReturn` 1

  -- Issue #5277: Dogs of War's The Beast in a Cowl of Crimson cancels its own
  -- defeat and moves itself to the Catacombs of Kom el Shoqafa. Defeating it
  -- while it is already at the Catacombs used to fire the enters-your-location
  -- windows for a move that never happened.
  it "does not trigger when an enemy is moved to the location it already occupies" . gameTest $ \self -> do
    here <- testLocation
    self `moveTo` here
    enemy <- testEnemy
    enemy `spawnAt` here
    loadDeck self [Cards.pursued]
    drawCards self 1
    run . Move =<< move (toSource enemy) (toTarget enemy) (toId here)
    assertNoPendingQuestion
    self.horror `shouldReturn` 0
