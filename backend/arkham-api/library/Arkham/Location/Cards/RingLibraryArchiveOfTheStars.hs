module Arkham.Location.Cards.RingLibraryArchiveOfTheStars (ringLibraryArchiveOfTheStars) where

import Arkham.Ability
import Arkham.Classes.HasQueue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (Discarded)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Passageway))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype RingLibraryArchiveOfTheStars = RingLibraryArchiveOfTheStars LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ringLibraryArchiveOfTheStars :: LocationCard RingLibraryArchiveOfTheStars
ringLibraryArchiveOfTheStars = location RingLibraryArchiveOfTheStars Cards.ringLibraryArchiveOfTheStars 2 (Static 2)

instance HasAbilities RingLibraryArchiveOfTheStars where
  getAbilities (RingLibraryArchiveOfTheStars a) =
    extendRevealed
      a
      [ -- One response to the whole discard, taken before any card leaves the
        -- encounter deck. Reactions are already PlayerLimit PerWindow 1, so
        -- moving off the per-card 'Discarded' window (which re-offered this
        -- between every single card) is what holds it to a single +2 per batch.
        reaction
          a
          1
          NoRestriction
          (HandDiscardCost 1 #any)
          (WouldDiscardTopOfEncounterDeck #when You AnySource)
      , restricted
          a
          2
          (Here <> exists (not_ (be a) <> RevealedLocation <> LocationWithTrait Passageway))
          actionAbility
      ]

instance RunMessage RingLibraryArchiveOfTheStars where
  runMessage msg l@(RingLibraryArchiveOfTheStars attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      -- "Discard 2 additional cards." Grow the pending discard rather than
      -- starting a second one: the effect that asked for the discard treats the
      -- batch as a unit (Escape the Tower draws a Glyph and every Omen from what
      -- it discarded), so cards split off into their own discard would never
      -- reach it. The queued message is still sitting behind this window check,
      -- so it can be rewritten before a single card moves.
      for_ ws \w -> case windowType w of
        Window.WouldDiscardTopOfEncounterDeck iid source _ -> lift do
          replaceMessageMatching
            ( \case
                DiscardTopOfEncounterDeckWithDiscardedCards iid' _ source' _ [] ->
                  iid' == iid && source' == source
                _ -> False
            )
            ( \case
                DiscardTopOfEncounterDeckWithDiscardedCards iid' n source' mtarget cards ->
                  [DiscardTopOfEncounterDeckWithDiscardedCards iid' (n + 2) source' mtarget cards]
                other -> [other]
            )
        _ -> pure ()
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      passageways <- select $ not_ (be attrs) <> RevealedLocation <> LocationWithTrait Passageway
      chooseTargetM iid passageways $ moveTo (attrs.ability 2) iid
      pure l
    _ -> RingLibraryArchiveOfTheStars <$> liftRunMessage msg attrs
