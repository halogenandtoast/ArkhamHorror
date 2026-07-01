module Arkham.Story.Cards.UnfinishedBusinessSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Placement
import Arkham.Story.Cards qualified as Stories
import TestImport qualified as TI
import TestImport.New

spec :: Spec
spec = describe "Unfinished Business" $ do
  it "links each story side back to its Heretic enemy side" do
    map (.otherSide)
      [ Stories.unfinishedBusiness_B
      , Stories.unfinishedBusiness_D
      , Stories.unfinishedBusiness_F
      , Stories.unfinishedBusiness_H
      , Stories.unfinishedBusiness_J
      , Stories.unfinishedBusiness_L
      ]
      `TI.shouldBe` map
        Just
        [ "05178a"
        , "05178c"
        , "05178e"
        , "05178g"
        , "05178i"
        , "05178k"
        ]

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
