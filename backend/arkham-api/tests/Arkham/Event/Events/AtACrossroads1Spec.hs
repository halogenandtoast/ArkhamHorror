module Arkham.Event.Events.AtACrossroads1Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Arkham.Matcher (assetIs, eventIs)
import Data.Text qualified as T
import Helpers.Message qualified
import TestImport.New

spec :: Spec
spec = describe "At a Crossroads (1)" do
  -- Regression for #4867 ("Card duplicated?"). At a Crossroads is drawn
  -- simultaneously with other cards; choosing the "take an action" branch of its
  -- revelation lets the player relocate one of the just-drawn cards (play it, or
  -- have it discarded) before that card's own deferred draw handler runs. The
  -- handler then used to finalize the draw anyway -- re-adding the card to hand
  -- (a phantom duplicate of the in-play copy) and stripping it out of the discard
  -- (orphaning it).

  it "does not duplicate a card played during its revelation" . gameTest $ \self -> do
    let setOwner' = setPlayerCardOwner (toId self)
    atACrossroads <- genPlayerCardWith Events.atACrossroads1 setOwner'
    scavenging <- genPlayerCardWith Assets.scavenging setOwner'
    filler <- genPlayerCardWith Assets.flashlight setOwner'
    withProp @"resources" 5 self
    -- At a Crossroads on top so its revelation resolves before Scavenging's
    -- deferred draw handler; the filler keeps the deck non-empty.
    withProp @"deck" (Deck [atACrossroads, scavenging, filler]) self
    drawCards self 2
    chooseTarget self -- the investigator affected by the revelation
    chooseOptionMatching "take an action" \case
      Label lbl _ -> "act" `T.isInfixOf` lbl
      _ -> False
    chooseTarget (toCardId scavenging) -- play Scavenging during the granted action
    assertAny $ assetIs Assets.scavenging
    self.hand `shouldSatisfyM` notElem (toCard scavenging)

  it "does not lose a card discarded during its revelation" . gameTest $ \self -> do
    let setOwner' = setPlayerCardOwner (toId self)
    atACrossroads <- genPlayerCardWith Events.atACrossroads1 setOwner'
    emergencyCache <- genPlayerCardWith Events.emergencyCache setOwner'
    filler <- genPlayerCardWith Assets.flashlight setOwner'
    withProp @"deck" (Deck [atACrossroads, emergencyCache, filler]) self
    drawCards self 2
    chooseTarget self
    chooseOptionMatching "take an action" \case
      Label lbl _ -> "act" `T.isInfixOf` lbl
      _ -> False
    -- Playing Emergency Cache sends it to the discard before its deferred draw
    -- handler runs; it must remain in the discard, exactly once, and not in hand.
    chooseTarget (toCardId emergencyCache)
    asDefs self.discard `shouldSatisfyM` ((== 1) . length . filter (== Events.emergencyCache))
    asDefs self.hand `shouldSatisfyM` notElem Events.emergencyCache

  -- #5974, FAQ 2.25: "If a Dilemma card is drawn while resolving a skill test,
  -- resolve the skill test in full before resolving the Dilemma's Revelation
  -- effect." Its revelation hands an investigator an action as if it were their
  -- turn, which has no coherent meaning inside a live test.
  faq "2.25" $ it "does not resolve its revelation during a skill test" . gameTest $ \self -> do
    let setOwner' = setPlayerCardOwner (toId self)
    atACrossroads <- genPlayerCardWith Events.atACrossroads1 setOwner'
    filler <- genPlayerCardWith Assets.flashlight setOwner'
    withProp @"deck" (Deck [atACrossroads, filler]) self
    sid <- getRandom
    -- Deliberately not runSkillTest: the test must still be live for the draw.
    run $ Helpers.Message.beginSkillTest sid self #intellect 2
    peekQueue `shouldSatisfyM` elem EndSkillTestWindow
    drawCards self 1

    -- Its revelation did resolve -- it is not lost -- but only after the test's own
    -- end-of-test sentinel was consumed. Had it cut in line, the ask it parks would
    -- have arrived with that sentinel still sitting in the queue.
    assertAny $ eventIs Events.atACrossroads1
    peekQueue `shouldSatisfyM` notElem EndSkillTestWindow
