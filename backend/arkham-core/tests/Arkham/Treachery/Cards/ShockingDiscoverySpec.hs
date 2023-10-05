module Arkham.Treachery.Cards.ShockingDiscoverySpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Investigator.Cards (mandyThompson)
import Arkham.Investigator.Types (InvestigatorAttrs (investigatorFoundCards))
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

{-
Forced - When you search your deck and this card is among the searched cards: Discard it. Cancel the search and all of its effects. Shuffle the searched deck. Draw the top card of the encounter deck.
-}

spec :: Spec
spec = describe "Shocking Discovery" do
  context "revelation" do
    it "shuffles back into your deck" . gameTestWith mandyThompson $ \self -> do
      flashlight <- genPlayerCard Assets.flashlight
      withProp @"deck" (Deck [flashlight]) self
      self `drawsCard` Treacheries.shockingDiscovery
      asDefs self.deck `shouldMatchListM` [Treacheries.shockingDiscovery, Assets.flashlight]

    it "if you cannot, discard it and draw the top card of the encounter deck" . gameTestWith mandyThompson $ \self -> do
      location <- testLocation
      self `moveTo` location
      card <- genEncounterCard Cards.swarmOfRats
      run $ SetEncounterDeck (Deck [card])
      withProp @"deck" (Deck []) self
      self `drawsCard` Treacheries.shockingDiscovery
      asDefs self.discard `shouldReturn` [Treacheries.shockingDiscovery]
      assertAny $ enemyIs Cards.swarmOfRats

  context "forced ability" do
    context "when you search your deck and this card is among the searched cards" do
      it "discard it, cancel the search and all of its effects. Draw the top card of the encounter deck" . gameTestWith mandyThompson $ \self -> do
        location <- testLocation & prop @"clues" 1
        shockingDiscovery <- genPlayerCard Treacheries.shockingDiscovery
        otherCards <- testPlayerCards 10
        cards <- shuffleM (shockingDiscovery : otherCards)
        rats <- genEncounterCard Cards.swarmOfRats
        run $ SetEncounterDeck (Deck [rats])
        withProp @"deck" (Deck cards) self
        self `moveTo` location
        run
          $ search (toId self) (TestSource mempty) (toId self) [fromDeck] AnyCard (DrawFound (toId self) 1)
        skip -- do not use mandy's ability
        useForcedAbility
        asDefs self.discard `shouldReturn` [Treacheries.shockingDiscovery]
        self.hand `shouldReturn` []
        (unDeck <$> self.deck) `shouldMatchListM` otherCards
        (attr investigatorFoundCards <$> getInvestigator (toId self)) `shouldReturn` mempty
        assertAny $ enemyIs Cards.swarmOfRats
