module Arkham.Act.Cards.BanishHim (banishHim) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Trait (Trait (Cthulhu))

newtype BanishHim = BanishHim ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

banishHim :: ActCard BanishHim
banishHim = act (1, A) BanishHim Cards.banishHim Nothing

instance HasAbilities BanishHim where
  getAbilities (BanishHim a)
    | onSide A a =
        [ -- Fast: deal +1 damage with your attacks against Cthulhu this round.
          -- TODO: the real cost (spending a sigil) and the Cthulhu-Board restriction
          -- have no engine support yet; modeled as a free, once-per-round boost.
          playerLimit PerRound
            $ restricted a 1 (DuringTurn You)
            $ FastAbility Free
        , -- Objective: when three of Cthulhu's facets have been banished to the
          -- victory display, advance to flip Cthulhu's final form.
          restricted a 2 (InVictoryDisplay (CardWithTrait Cthulhu) (atLeast 3))
            $ Objective
            $ forced
            $ RoundEnds #when
        ]
  getAbilities _ = []

instance RunMessage BanishHim where
  runMessage msg a@(BanishHim attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      roundModifier (attrs.ability 1) iid (DamageDealt 1)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide A attrs -> True) _ _ -> do
      -- Cthulhu's Rage ratchets up. If it is still low the act simply flips back;
      -- once it crests, Cthulhu is enraged and the final seal is contested.
      scenarioSpecific "increaseCthulhuRage" ()
      rage <- getCthulhuRage
      if rage <= 3
        then do
          -- TODO: flip the act back to side A (the Cthulhu Board resets); no clean
          -- "revert act" primitive, so this is scaffolded as a re-advance for now.
          pure a
        else do
          -- Rage is set to 5 (4 with one or two investigators).
          playerCount <- getPlayerCount
          scenarioSpecific "setCthulhuRage" (if playerCount <= 2 then 4 :: Int else 5)

          -- TODO: flip each Cthulhu enemy to its Enraged side. This is the Cthulhu
          -- Board flip and has no clean primitive (the facets are double-sided cards
          -- and the board-level swap is bespoke).

          -- Place clues on each revealed non-victory location up to its clue value.
          locations <- select $ RevealedLocation <> not_ LocationWithVictory
          for_ locations $ placeCluesUpToClueValue (attrs.ability 2)

          -- TODO: each investigator may take a set-aside Ally story asset into play
          -- without using an ally slot. Deferred with the rest of the ally setup.

          -- TODO: replace this act with "The Final Seal" (11691b) as a special
          -- agenda and place 1 [per_investigator] doom on it. The act flips to its B
          -- side here, but the act->special-agenda swap and the per-investigator doom
          -- on that agenda have no clean primitive yet.
          pure a
    _ -> BanishHim <$> liftRunMessage msg attrs
