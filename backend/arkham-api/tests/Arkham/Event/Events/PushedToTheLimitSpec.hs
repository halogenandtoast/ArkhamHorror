module Arkham.Event.Events.PushedToTheLimitSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Pushed to the Limit" do
  it "shuffles the chosen asset into the deck" . gameTest $ \self -> do
    flashlight <- genPlayerCardWith Assets.flashlight (setPlayerCardOwner (toId self))
    withProp @"discard" [flashlight] self
    withDeck self [Assets.machete]
    location <- testLocation
    self `moveTo` location
    setChaosTokens [Zero]

    playEvent self Events.pushedToTheLimit
    chooseTarget flashlight
    chooseOnlyOption "investigate with the Flashlight, ignoring its supply cost"
    startSkillTest
    applyResults

    asDefs self.deck `shouldMatchListM` [Assets.flashlight, Assets.machete]
    asDefs self.discard `shouldReturn` [Events.pushedToTheLimit]

  {- FAQ (1.13): a single card cannot be shuffled into an empty player deck, so the
  Flashlight stays in the discard pile. The ability still resolves. A player read
  "shuffle the chosen asset into your deck", watched it not happen and reported a
  bug against correct behaviour, so the refusal is narrated rather than silent
  (#6046). -}
  it "leaves the chosen asset in the discard pile when the deck is empty" . gameTest $ \self -> do
    flashlight <- genPlayerCardWith Assets.flashlight (setPlayerCardOwner (toId self))
    withProp @"discard" [flashlight] self
    withDeck self ([] :: [CardDef])
    location <- testLocation
    self `moveTo` location
    setChaosTokens [Zero]

    playEvent self Events.pushedToTheLimit
    chooseTarget flashlight
    chooseOnlyOption "investigate with the Flashlight, ignoring its supply cost"
    startSkillTest
    applyResults

    fmap length self.deck `shouldReturn` 0
    asDefs self.discard `shouldMatchListM` [Assets.flashlight, Events.pushedToTheLimit]
