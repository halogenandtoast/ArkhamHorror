module Arkham.Event.Events.Respite2Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Arkham.Skill.Cards qualified as Skills
import TestImport.New

spec :: Spec
spec = describe "Respite (2)" do
  it "shuffles the chosen cards into your deck as a group before drawing" . gameTest $ \self -> do
    guts <- genPlayerCardWith Skills.guts (setPlayerCardOwner (toId self))
    overpower <- genPlayerCardWith Skills.overpower (setPlayerCardOwner (toId self))
    unexpectedCourage <- genPlayerCardWith Skills.unexpectedCourage (setPlayerCardOwner (toId self))
    withProp @"discard" [guts, overpower, unexpectedCourage] self
    withDeck self ([] :: [CardDef])

    playEvent self Events.respite2
    chooseTarget guts
    chooseTarget overpower
    chooseTarget unexpectedCourage

    -- FAQ (1.13) only forbids shuffling a *single* card into an empty deck, so
    -- all three go in and the draw never sees an empty deck
    fmap length self.deck `shouldReturn` 2
    fmap length self.hand `shouldReturn` 1
    self.horror `shouldReturn` 0
    asDefs self.discard `shouldReturn` [Events.respite2]

  it "can only choose level 0 event and skill cards" . gameTest $ \self -> do
    otherRespite <- genPlayerCardWith Events.respite2 (setPlayerCardOwner (toId self))
    withProp @"discard" [otherRespite] self
    withDeck self [Assets.flashlight]

    playEvent self Events.respite2

    -- nothing eligible to choose, so we go straight to the draw
    asDefs self.hand `shouldReturn` [Assets.flashlight]
    asDefs self.discard `shouldReturn` [Events.respite2, Events.respite2]
