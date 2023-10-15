module Arkham.Asset.Cards.CrystallizerOfDreamsSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Crystallizer of Dreams" do
  context
    "As an additional cost to play this card, you must search your bonded cards for 1 copy of Guardian of the Crystallizer and shuffle it into your deck."
    do
      it "can be played if cards are in bonded" . gameTest $ \self -> do
        crystallizerOfDreams <- genCard Assets.crystallizerOfDreams
        guardianOfTheCrystallizer <- genCard Enemies.guardianOfTheCrystallizer
        withProp @"resources" 1 self
        withProp @"bonded" [guardianOfTheCrystallizer] self
        withProp @"hand" [crystallizerOfDreams] self
        duringTurn self do
          self.playableCards `shouldReturn` [crystallizerOfDreams]
          self `playCard` crystallizerOfDreams
          (unDeck <$> self.deck) `shouldReturn` onlyPlayerCards [guardianOfTheCrystallizer]

      it "can't be played if cards are not in bonded" . gameTest $ \self -> do
        crystallizerOfDreams <- genCard Assets.crystallizerOfDreams
        withProp @"resources" 1 self
        withProp @"hand" [crystallizerOfDreams] self
        duringTurn self do
          self.playableCards `shouldReturn` []

  context "After you play an event" do
    it
      "Attach it facedown to Crystallizer of Dreams instead of discarding it"
      . gameTest
      $ \self -> do
        crystallizerOfDreams <- self `putAssetIntoPlay` Assets.crystallizerOfDreams
        self `playEvent` Events.emergencyCache
        useReaction
        self.discard `shouldReturn` []
        asDefs crystallizerOfDreams.cardsUnderneath `shouldReturn` [Events.emergencyCache]

    it "(to a maximum of 5 attached events)." . gameTest $ \self ->
      do
        crystallizerOfDreams <- self `putAssetIntoPlay` Assets.crystallizerOfDreams
        replicateM_ 5 do
          self `playEvent` Events.emergencyCache
          useReaction
        self `playEvent` Events.emergencyCache
        asDefs self.discard `shouldReturn` [Events.emergencyCache]
        asDefs crystallizerOfDreams.cardsUnderneath `shouldReturn` replicate 5 Events.emergencyCache

    it "Attached events may be committed to skill tests as if they were in your hand." . gameTest $ \self -> do
      location <- testLocation & prop @"clues" 1
      self `moveTo` location
      withProp @"intellect" 0 self
      crystallizerOfDreams <- self `putAssetIntoPlay` Assets.crystallizerOfDreams
      duringTurn self $ do
        self `playEvent` Events.workingAHunch
        useReaction
        [workingAHunch] <- crystallizerOfDreams.cardsUnderneath
        run $ beginSkillTest self #intellect 2
        commit workingAHunch
        startSkillTest
        assertPassedSkillTest
