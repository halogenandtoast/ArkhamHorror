module Arkham.Event.Cards.OccultEvidenceSpec (spec) where

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards (mandyThompson)
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "Occult Evidence" do
  it "Shuffle Occult Evidence into your deck." . gameTestWith mandyThompson $ \self -> do
    self `playEvent` Events.occultEvidence
    asDefs self.deck `shouldReturn` [Events.occultEvidence]

  context "When you search your deck and Occult Evidence is among the searched cards" do
    it "reveal it: Draw it and discover 1 clue at your location" . gameTestWith mandyThompson $ \self -> do
      location <- testLocation & prop @"clues" 1
      occultEvidence <- genPlayerCard Events.occultEvidence
      otherCards <- testPlayerCards 10
      cards <- shuffleM (occultEvidence : otherCards)
      withProp @"deck" (Deck cards) self
      self `moveTo` location
      run
        $ search (toId self) (TestSource mempty) (toId self) [fromDeck] AnyCard (DrawFound (toId self) 1)
      skip -- do not use mandy's ability
      useReaction
      self.clues `shouldReturn` 1
      location.clues `shouldReturn` 0
      self.hand `shouldReturn` [toCard occultEvidence]
