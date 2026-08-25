module Arkham.Homebrew.DarkMatter.Treacheries.DarkReflectionsZealot (
  darkReflectionsZealot,
) where

import Arkham.Ability
import Arkham.Discard
import Arkham.Helpers.Message.Discard (discardFromHand)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted hiding (DeckHasNoCards)

{- | "Hidden. Peril.
Revelation - Secretly add this card to your hand.
Forced - After you reshuffle your deck because there are no cards in it: Discard
this card and take 3 damage.
[action] Choose an investigator at your location to choose and discard 3 cards
from their hand: Discard Dark Reflections (Zealot)."
-}
newtype DarkReflectionsZealot = DarkReflectionsZealot TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkReflectionsZealot :: TreacheryCard DarkReflectionsZealot
darkReflectionsZealot = treachery DarkReflectionsZealot Cards.darkReflectionsZealot

instance HasAbilities DarkReflectionsZealot where
  getAbilities (DarkReflectionsZealot a) =
    [ restricted a 1 InYourHand $ forced $ DeckHasNoCards #after You
    , restricted a 2 (InYourHand <> exists (InvestigatorAt YourLocation)) actionAbility
    ]

instance RunMessage DarkReflectionsZealot where
  runMessage msg t@(DarkReflectionsZealot attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid attrs 3
      toDiscardBy iid attrs attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      victims <- select $ InvestigatorAt YourLocation
      chooseTargetM iid victims \victim ->
        push $ toMessage $ discardFromHand victim (attrs.ability 2) DiscardChoose 3
      toDiscardBy iid attrs attrs
      pure t
    _ -> DarkReflectionsZealot <$> liftRunMessage msg attrs
