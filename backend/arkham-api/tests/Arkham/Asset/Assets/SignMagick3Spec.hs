module Arkham.Asset.Assets.SignMagick3Spec (spec) where

import Arkham.Ability.Types (Ability (..), abilitySource)
import Arkham.Asset.Cards qualified as Assets
import TestImport.New

-- | Regression coverage for issue #4905.
--
-- Upgraded Sign Magick (3) can target True Magick: Reworking Reality (5) so
-- that, after activating an in-play [Spell] [action], the player may use Sign
-- Magick to activate True Magick (at action-cost 0) and resolve the [action]
-- ability of an in-hand [Spell] asset without paying its action cost.
--
-- The board for every case: the investigator controls Sign Magick (3) and True
-- Magick (5) in play, plus an in-play Clarity of Mind (a [Spell] with an
-- [action]) whose activation opens the "after you activate an [action] on a
-- [Spell]" window that triggers Sign Magick's reaction. The lever that varies
-- between the happy path and the guard is whether there is a castable in-hand
-- [Spell] asset for True Magick to borrow.
spec :: Spec
spec = describe "Sign Magick (3)" $ do
  context "with True Magick (5) (issue #4905)" $ do
    -- CASE 1: the #4905 repro. With a castable in-hand [Spell] asset, activating
    -- the in-play spell's [action] must surface Sign Magick's reaction, which in
    -- turn offers True Magick's borrowed [action] at action-cost 0, which reveals
    -- and resolves the in-hand spell. Before the fix the reaction was suppressed
    -- entirely (True Magick never read as a Spell, so Sign Magick could not
    -- target it).
    it "offers True Magick's borrowed in-hand [action] after activating an in-play Spell" . gameTest $ \self -> do
      -- Note: the ActivateAbility #after window (where Sign Magick reacts) fires
      -- AFTER the in-play Clarity of Mind resolves its heal, so self needs enough
      -- horror left over for the in-hand Clarity of Mind to still be castable when
      -- the reaction is evaluated -- otherwise True Magick has nothing to borrow.
      withProp @"horror" 5 self
      location <- testLocation
      self `moveTo` location

      signMagick <- self `putAssetIntoPlay` Assets.signMagick3
      trueMagick <- self `putAssetIntoPlay` Assets.trueMagickReworkingReality5
      clarityInPlay <- self `putAssetIntoPlay` Assets.clarityOfMind

      -- castable in-hand [Spell] asset with an [action] (a second Clarity of Mind)
      inHandSpell <- genMyCard self Assets.clarityOfMind3
      addToHand self inHandSpell

      -- activate the in-play Clarity of Mind [action]; this opens the
      -- ActivateAbility #after window Sign Magick reacts to
      [clarityAction] <- self `getActionsFrom` clarityInPlay
      self `useAbility` clarityAction

      -- Sign Magick's reaction is available (the core #4905 regression: before the
      -- fix True Magick never read as a Spell, so Sign Magick had no legal target
      -- and the reaction was suppressed entirely).
      useReactionOf signMagick

      -- Resolving Sign Magick then offers True Magick's borrowed in-hand spell
      -- [action] at action-cost 0. The offered options are BOTH True Magick's own
      -- tooltip action (cardCode 08070, plain AssetSource) and the in-hand spell
      -- re-sourced onto True Magick via a ProxySource (cardCode 51008). Selecting
      -- the borrowed one proves the whole chain (Sign Magick -> True Magick proxy
      -- -> in-hand spell) is wired. Disambiguate by the ProxySource whose unwrapped
      -- .asset is the True Magick asset id and whose abilityCardCode is the in-hand
      -- spell's. (We stop here: actually resolving the borrowed Clarity drags in its
      -- heal arithmetic, which the manual arkham-replay verification already covers.)
      chooseOptionMatching "borrowed in-hand spell action" $ \case
        AbilityLabel {ability} -> case abilitySource ability of
          ProxySource {} ->
            (abilitySource ability).asset == Just trueMagick
              && ability.abilityCardCode == toCardCode Assets.clarityOfMind3
          _ -> False
        _ -> False

    -- CASE 2: the over-trigger guard. Same board but the hand holds no castable
    -- in-hand [Spell] asset, so True Magick has nothing to borrow. True Magick
    -- must NOT read as a Spell/Ritual and Sign Magick's reaction must NOT be
    -- offered when the in-play spell is activated.
    it "does not offer its reaction when there is no castable in-hand Spell" . gameTest $ \self -> do
      withProp @"horror" 2 self
      location <- testLocation
      self `moveTo` location

      _signMagick <- self `putAssetIntoPlay` Assets.signMagick3
      _trueMagick <- self `putAssetIntoPlay` Assets.trueMagickReworkingReality5
      clarityInPlay <- self `putAssetIntoPlay` Assets.clarityOfMind
      -- hand intentionally left without a [Spell] asset

      [clarityAction] <- self `getActionsFrom` clarityInPlay
      self `useAbility` clarityAction
      assertNoReaction
