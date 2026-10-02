module Arkham.Asset.Assets.SignMagick3Spec (spec) where

import Arkham.Ability.Types (Ability (..), abilitySource)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Window (defaultWindows)
import TestImport.New

{- | Regression coverage for issue #4905.

Upgraded Sign Magick (3) can target True Magick: Reworking Reality (5) so
that, after activating an in-play [Spell] [action], the player may use Sign
Magick to activate True Magick (at action-cost 0) and resolve the [action]
ability of an in-hand [Spell] asset without paying its action cost.

The board for every case: the investigator controls Sign Magick (3) and True
Magick (5) in play, plus an in-play Clarity of Mind (a [Spell] with an
[action]) whose activation opens the "after you activate an [action] on a
[Spell]" window that triggers Sign Magick's reaction. The lever that varies
between the happy path and the guard is whether there is a castable in-hand
[Spell] asset for True Magick to borrow.
-}
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

      -- Resolving Sign Magick offers the in-hand spell itself, re-sourced onto True
      -- Magick by getTrueMagickInHandAbilities and revealed as it is chosen. Under the
      -- project ruling the asset you activate is the revealed [Spell] True Magick became
      -- a copy of, not True Magick, so the spell is what the list names. (We stop here:
      -- actually resolving the borrowed Clarity drags in its heal arithmetic, which the
      -- manual arkham-replay verification already covers.)
      chooseOptionMatching "borrowed in-hand spell action, revealed first" $ \case
        AbilityLabel {ability, before = beforeMsgs} -> case abilitySource ability of
          ProxySource {} ->
            (abilitySource ability).asset
              == Just trueMagick
              && ability.abilityCardCode
              == toCardCode Assets.clarityOfMind3
              && beforeMsgs
              == [RevealCard (toCardId inHandSpell)]
          _ -> False
        _ -> False

    -- CASE 1b (issue #5801): Sign Magick grants an [action] activation only. The FAQ
    -- (February 2025) lets it treat True Magick as a revealed [Spell] from hand, but it
    -- does not widen Sign Magick's own "Activate an [action] ability". Scrying (3) is a
    -- [Spell] asset whose only ability is [fast], so it must not be offered.
    it "does not offer a borrowed in-hand Spell whose only ability is [fast]" . gameTest $ \self -> do
      withProp @"horror" 5 self
      location <- testLocation
      self `moveTo` location

      signMagick <- self `putAssetIntoPlay` Assets.signMagick3
      _trueMagick <- self `putAssetIntoPlay` Assets.trueMagickReworkingReality5
      clarityInPlay <- self `putAssetIntoPlay` Assets.clarityOfMind

      -- the [action] spell, which both lets True Magick read as a Spell and is the one
      -- legal entry Sign Magick may offer
      actionSpell <- self `genMyCard` Assets.clarityOfMind3
      addToHand self actionSpell
      -- ...and a [fast]-only one, which must not be reachable
      fastSpell <- self `genMyCard` Assets.scrying3
      addToHand self fastSpell

      [clarityAction] <- self `getActionsFrom` clarityInPlay
      self `useAbility` clarityAction
      useReactionOf signMagick

      -- chooseOnlyOption fails unless there is EXACTLY one option, so this is the
      -- assertion that Scrying was excluded
      chooseOnlyOption "the only legal entry is the borrowed [action] spell"

    -- CASE 1c (project ruling, issue #5801): what you activate through True Magick is the
    -- revealed [Spell] it became a copy of -- name included (FAQ v2.5 Q69) -- not True
    -- Magick itself. So activating True Magick as one spell leaves True Magick available
    -- to Sign Magick as "a different [[Spell]] asset", for a DIFFERENT spell. The spell
    -- just revealed is the same asset and must not come back round.
    it "offers a different in-hand Spell after True Magick was itself activated" . gameTest $ \self -> do
      withProp @"willpower" 5 self
      -- horror to heal, so the OTHER borrowed spell (Clarity of Mind (3)) is performable
      withProp @"horror" 5 self
      location <- testLocation & prop @"clues" 2 & prop @"shroud" 0
      setChaosTokens [Zero]
      self `moveTo` location

      signMagick <- self `putAssetIntoPlay` Assets.signMagick3
      trueMagick <- self `putAssetIntoPlay` Assets.trueMagickReworkingReality5

      secondSight <- self `genMyCard` Assets.secondSight
      addToHand self secondSight
      otherSpell <- self `genMyCard` Assets.clarityOfMind3
      addToHand self otherSpell

      [tmAction] <- self `getActionsFrom` trueMagick
      run $ UseAbility (toId self) tmAction (defaultWindows $ toId self)
      chooseTarget (toCardId secondSight)
      chooseOnlyOption "resolve the borrowed Second Sight investigate"
      startSkillTest
      applyResults
      -- decline Second Sight's "spend 1 charge for an extra clue", so True Magick keeps
      -- the charge Clarity of Mind (3) needs
      clickLabel "$label.skip"

      -- pre-ruling: not offered at all, because ExcludeWindowAssetExists looked through
      -- the proxy and ruled True Magick out as "not a different asset"
      useReactionOf signMagick

      -- chooseOnlyOption fails unless there is EXACTLY one option, so this is the
      -- assertion that Second Sight -- the asset we just activated -- is not offered again
      chooseOnlyOption "the only different in-hand Spell is Clarity of Mind (3)"

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
