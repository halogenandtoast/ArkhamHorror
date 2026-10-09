module Arkham.Homebrew.TheSymphonyOfErichZann.Treacheries.StuckInYourHead (stuckInYourHead) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Target
import Arkham.Treachery.Import.Lifted

newtype StuckInYourHead = StuckInYourHead TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stuckInYourHead :: TreacheryCard StuckInYourHead
stuckInYourHead = treachery StuckInYourHead Cards.stuckInYourHead

instance HasModifiersFor StuckInYourHead where
  -- "Stuck in Your Head counts as 3 cards instead of 1 while checking your hand size."
  getModifiersFor (StuckInYourHead a) =
    modified_ a (CardIdTarget $ toCardId a) [HandSizeCardCount 3]

instance HasAbilities StuckInYourHead where
  {- "After you discard 1 or more cards from your hand during the upkeep phase:
  Draw the top card of the encounter deck and discard Stuck in Your Head." -}
  getAbilities (StuckInYourHead a) =
    [ restricted a 1 InYourHand
        $ forced
        $ DiscardedFromHand #after You AnySource #any
    ]

instance RunMessage StuckInYourHead where
  runMessage msg t@(StuckInYourHead attrs) = runQueueT $ case msg of
    {- "Revelation - Secretly add this card to your hand." `addToHand` would
    loop: `handleDoAddToHand` re-draws any card with a revelation, so this one
    would reveal itself forever. Hidden weaknesses are placed instead. -}
    Revelation iid (isSource attrs -> True) -> do
      placeTreachery attrs (HiddenInHand iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCard iid (attrs.ability 1)
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> StuckInYourHead <$> liftRunMessage msg attrs
