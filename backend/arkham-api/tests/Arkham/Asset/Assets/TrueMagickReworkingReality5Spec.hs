module Arkham.Asset.Assets.TrueMagickReworkingReality5Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher
import Arkham.Trait (Trait (Spell))
import Arkham.Window (defaultWindows)
import TestImport.New

{- | The test harness answers a question by pushing the answer's messages
directly, without the @ClearUI@ that the API pushes ahead of every accepted
answer (see Api.Handler.Arkham.Games.Shared). So when an answer creates no new
Ask -- exactly the case for declining a triggers window -- the answered
question lingers in gameQuestion and assertNoReaction cannot tell "no further
window" from "stale window". Push it ourselves.
-}
skipWindow :: HasCallStack => TestAppT ()
skipWindow = push ClearUI >> skip

{- | Coverage for the trait-leak guard and the Twila non-regression around
True Magick: Reworking Reality (5) (issue #4905).
-}
spec :: Spec
spec = describe "True Magick: Reworking Reality (5)" $ do
  -- CASE 3: trait non-leak. With no castable in-hand [Spell] asset, True Magick
  -- (Item/Relic/Tome) must NOT read as a Spell at rest -- otherwise "all your
  -- Spell assets" effects would wrongly sweep it up.
  it "does not read as a Spell at rest with no castable in-hand Spell" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    _trueMagick <- self `putAssetIntoPlay` Assets.trueMagickReworkingReality5
    -- hand intentionally empty of [Spell] assets
    assertNone $ assetIs Assets.trueMagickReworkingReality5 <> AssetWithTrait Spell

  -- CASE 4: Twila Katherine Price (3) non-regression. Casting a charge-spending
  -- in-hand [Spell] asset through True Magick still fires Twila's
  -- "SpentUses #after ... Spell" reaction, because while True Magick borrows the
  -- spell it reads with the spell's traits, so the spent charge counts as spent
  -- on a [Spell] asset.
  --
  -- We drive True Magick's own [action] via getActionsFrom (so it carries proper
  -- windows -- a bare UseAbility passes empty windows and makes the in-hand action
  -- read as non-performable). No Sign Magick / in-play opener is needed: this
  -- isolates the through-True-Magick charge spend that Twila reacts to.
  it "still fires Twila's Spell reaction when a charge-spending in-hand Spell is cast through it" . gameTest $ \self -> do
    -- horror to heal so the borrowed Clarity of Mind [action] is performable
    withProp @"horror" 5 self
    location <- testLocation
    self `moveTo` location

    twila <- self `putAssetIntoPlay` Assets.twilaKatherinePrice3
    trueMagick <- self `putAssetIntoPlay` Assets.trueMagickReworkingReality5

    -- charge-spending in-hand [Spell] asset (Clarity of Mind spends 1 charge)
    inHandSpell <- genMyCard self Assets.clarityOfMind
    addToHand self inHandSpell

    -- activate True Magick's own [action] (singleton: getActionsFrom filters to the
    -- AssetSource-sourced action, excluding the borrowed ProxySource ones), reveal
    -- the in-hand Clarity, and resolve its [action]. Its charge cost is paid from
    -- True Magick, which reads with the spell's traits while borrowing -- so the
    -- spend counts as spent on a [Spell].
    -- activate with real action windows: useAbility passes [] windows, which makes
    -- True Magick's in-hand performability re-check fail (an in-hand [action] needs
    -- a DuringYourAction window to read as performable).
    [tmAction] <- self `getActionsFrom` trueMagick
    run $ UseAbility (toId self) tmAction (defaultWindows $ toId self)
    chooseTarget (toCardId inHandSpell)
    chooseOnlyOption "resolve the borrowed Clarity of Mind [action]"

    -- Twila's reaction to the charge being spent on a [Spell] must still fire.
    useReactionOf twila

  -- CASE 5 (issue #5298): True Magick's own ability is only a wrapper that picks
  -- which borrowed in-hand ability to resolve -- the borrowed ability is the one
  -- real activation. Before the fix the wrapper was accounted for as an activate
  -- action of its own, so a single use recorded TWO activate actions.
  it "records the borrowed activation as a single activate action" . gameTest $ \self -> do
    withProp @"horror" 5 self
    location <- testLocation
    self `moveTo` location

    trueMagick <- self `putAssetIntoPlay` Assets.trueMagickReworkingReality5
    inHandSpell <- genMyCard self Assets.clarityOfMind
    addToHand self inHandSpell

    [tmAction] <- self `getActionsFrom` trueMagick
    run $ UseAbility (toId self) tmAction (defaultWindows $ toId self)
    chooseTarget (toCardId inHandSpell)
    chooseOnlyOption "resolve the borrowed Clarity of Mind [action]"

    -- pre-fix: [[Activate], [Activate]] -- the wrapper plus the borrowed ability
    fieldAssertLength InvestigatorActionsPerformed 1 self

  -- CASE 6 (issue #5298): the user-visible symptom of CASE 5. Two recorded
  -- activate actions in a row opened PerformedSameTypeOfAction, so Haste (2)
  -- offered a free repeat action off a single activation.
  it "does not trigger Haste (2) off a single borrowed activation" . gameTest $ \self -> do
    withProp @"horror" 5 self
    location <- testLocation
    self `moveTo` location

    _haste <- self `putAssetIntoPlay` Assets.haste2
    trueMagick <- self `putAssetIntoPlay` Assets.trueMagickReworkingReality5
    inHandSpell <- genMyCard self Assets.clarityOfMind
    addToHand self inHandSpell

    [tmAction] <- self `getActionsFrom` trueMagick
    run $ UseAbility (toId self) tmAction (defaultWindows $ toId self)
    chooseTarget (toCardId inHandSpell)
    chooseOnlyOption "resolve the borrowed Clarity of Mind [action]"

    assertNoReaction

  -- CASE 7 (issue #5298): the wrapper opened its own ActivateAbility #after
  -- window on top of the borrowed ability's, so Sign Magick (3) asked twice for
  -- one activation. Skipping the (single, correct) prompt must end the window.
  it "opens only one ActivateAbility window for Sign Magick (3)" . gameTest $ \self -> do
    withProp @"horror" 5 self
    location <- testLocation
    self `moveTo` location

    _signMagick <- self `putAssetIntoPlay` Assets.signMagick3
    trueMagick <- self `putAssetIntoPlay` Assets.trueMagickReworkingReality5
    -- a second in-play [Spell] with an [action], so Sign Magick has a legal
    -- target other than the window asset (True Magick itself)
    _clarityInPlay <- self `putAssetIntoPlay` Assets.clarityOfMind
    inHandSpell <- genMyCard self Assets.clarityOfMind3
    addToHand self inHandSpell

    [tmAction] <- self `getActionsFrom` trueMagick
    run $ UseAbility (toId self) tmAction (defaultWindows $ toId self)
    chooseTarget (toCardId inHandSpell)
    chooseOnlyOption "resolve the borrowed Clarity of Mind (3) [action]"

    -- decline the one correct prompt, from the borrowed ability's activation
    -- (`skip` fails outright if no triggers window is open at all)
    skipWindow
    -- pre-fix: a second, spurious prompt from the wrapper's own window
    assertNoReaction
