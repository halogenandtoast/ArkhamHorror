module Arkham.Story.Cards.UnfinishedBusinessSpec (spec) where

import Arkham.DefeatedBy
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.Placement
import Arkham.Story.Cards qualified as Stories
import Arkham.Window qualified as Window
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

  it "resolves into the defeating investigator's threat area when another investigator clicks the forced ability"
    $ gameTest
    $ \defeatingInvestigator -> do
      clickingInvestigator <- addInvestigator Investigators.rolandBanks
      location <- testLocationWith id
      run $ placedLocation location
      moveTo defeatingInvestigator location
      moveTo clickingInvestigator location

      heretic <- testEnemyWithDef Enemies.heretic_A id
      run $ PlaceEnemy (toId heretic) (AtLocation (toId location))

      let
        windows =
          [ Window.mkAfter
              $ Window.EnemyDefeated
                (Just $ toId defeatingInvestigator)
                (DefeatedByDamage $ InvestigatorSource $ toId defeatingInvestigator)
                (toId heretic)
          ]

      run $ UseCardAbility (toId clickingInvestigator) (toSource heretic) 2 windows NoPayment
      chooseOnlyOption "resolve Unfinished Business"

      assert
        $ selectAny
        $ StoryWithTitle "Unfinished Business"
        <> StoryWithPlacement (InThreatArea $ toId defeatingInvestigator)
      assertNone
        $ StoryWithTitle "Unfinished Business"
        <> StoryWithPlacement (InThreatArea $ toId clickingInvestigator)
