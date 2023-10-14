module Arkham.Event.Cards.AstoundingRevelationSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "Astounding Revelation" do
  context "When you search your deck and Astounding Revelation is among the searched cards" do
    context "discard it: Either" do
      it "gain 2 resources" . gameTest $ \self -> do
        astoundingRevelation <- genPlayerCard Events.astoundingRevelation
        withProp @"deck" (Deck [astoundingRevelation]) self
        run
          $ search (toId self) (TestSource mempty) (toTarget self) [fromDeck] AnyCard (DrawFound (toId self) 1)
        useReaction
        chooseOnlyOption "Take resources"
        self.resources `shouldReturn` 2
        self.discard `shouldReturn` [astoundingRevelation]

      it "or place 1 secret on an asset you control." . gameTest $ \self -> do
        forbiddenKnowledge <- self `putAssetIntoPlay` Assets.forbiddenKnowledge
        astoundingRevelation <- genPlayerCard Events.astoundingRevelation
        withProp @"deck" (Deck [astoundingRevelation]) self
        run
          $ search (toId self) (TestSource mempty) (toTarget self) [fromDeck] AnyCard (DrawFound (toId self) 1)
        useReaction
        chooseTarget forbiddenKnowledge
        self.resources `shouldReturn` 0
        self.discard `shouldReturn` [astoundingRevelation]
        forbiddenKnowledge.secrets `shouldReturn` 5

      it "(Max one Research ability per search.)" . gameTest $ \self -> do
        astoundingRevelation1 <- genPlayerCard Events.astoundingRevelation
        astoundingRevelation2 <- genPlayerCard Events.astoundingRevelation
        withProp @"deck" (Deck [astoundingRevelation1, astoundingRevelation2]) self
        run
          $ search (toId self) (TestSource mempty) (toTarget self) [fromDeck] AnyCard (DrawFound (toId self) 1)
        useReactionOf (EventSource $ EventId $ unsafeCardIdToUUID $ toCardId astoundingRevelation1)
        chooseOnlyOption "Take resources"
        chooseOnlyOption "Add other to hand"
        self.resources `shouldReturn` 2
        self.discard `shouldReturn` [astoundingRevelation1]
        self.hand `shouldReturn` [toCard astoundingRevelation2]
