module Arkham.Story.Cards.UnfinishedBusinessSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection (field)
import Arkham.Story.Types (Field (StoryFlipped))
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

  it "keeps its story side faceup after resolving into an investigator's threat area" $ gameTest $ \investigator -> do
    for_
      [ (Stories.unfinishedBusiness_B, "05178b")
      , (Stories.unfinishedBusiness_D, "05178d")
      , (Stories.unfinishedBusiness_F, "05178f")
      , (Stories.unfinishedBusiness_H, "05178h")
      , (Stories.returnToUnfinishedBusiness_38, "54038b")
      , (Stories.returnToUnfinishedBusiness_39, "54039b")
      ]
      \(storyCard, storyId) -> do
        card <- genCard storyCard
        run $ StoryMessage $ ReadStoryWithPlacement (toId investigator) card ResolveIt Nothing (InThreatArea $ toId investigator)

        field StoryFlipped storyId `shouldReturn` False
        chooseOnlyOption "resolve Unfinished Business"
        field StoryFlipped storyId `shouldReturn` False
