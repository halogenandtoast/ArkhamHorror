module Arkham.Event.Events.LieInWaitSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Lie in Wait" $ do
  faq "an aloof enemy cannot be attacked while unengaged, including via \"Fight\" triggered abilities" $ do
    it "does not offer its fight ability against an unengaged aloof enemy that enters" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      self `playEvent` Events.lieInWait
      whippoorwill <- testEnemyWithDef Enemies.whippoorwill id
      run $ EnemyEntered (toId whippoorwill) (toId location)
      assertNoReaction

    it "offers its fight ability against a non-aloof enemy that enters" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      self `playEvent` Events.lieInWait
      enemy <- testEnemy
      run $ EnemyEntered (toId enemy) (toId location)
      useReaction
