module Arkham.Asset.Assets.TrueMagickReworkingReality5Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher
import Arkham.Trait (Trait (Spell))
import Arkham.Window (defaultWindows)
import TestImport.New

-- | Coverage for the trait-leak guard and the Twila non-regression around
-- True Magick: Reworking Reality (5) (issue #4905).
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
