module Arkham.Story.Cards.UnfinishedBusinessSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Placement
import Arkham.Story.Cards qualified as Stories
import TestImport.New

spec :: Spec
spec = describe "Unfinished Business" $ do
  -- Errata (FAQ v1.6): "Keep this card in your threat area (this side faceup).
  -- If you are eliminated, flip it over." The card must flip back to its Heretic
  -- side rather than leaving play with the eliminated investigator.
  it "flips back to its Heretic side when the investigator is eliminated" $ gameTest $ \investigator -> do
    location <- testLocationWith id
    run $ placedLocation location
    moveTo investigator location
    card <- genCard Stories.unfinishedBusiness_B
    run $ StoryMessage $ PlaceStory card (InThreatArea (toId investigator))
    run $ Resign (toId investigator)
    chooseOnlyOption "flip Unfinished Business back over"
    assert $ selectAny (enemyIs Enemies.heretic_A)
