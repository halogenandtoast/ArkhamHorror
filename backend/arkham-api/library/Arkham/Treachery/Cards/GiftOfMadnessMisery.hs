module Arkham.Treachery.Cards.GiftOfMadnessMisery (giftOfMadnessMisery) where

import Arkham.Ability
import Arkham.Choose
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher hiding (
  PlaceUnderneath,
  TreacheryInHandOf,
  treacheryInHandOf,
 )
import Arkham.Placement
import Arkham.Scenario.Deck
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GiftOfMadnessMisery = GiftOfMadnessMisery TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

giftOfMadnessMisery :: TreacheryCard GiftOfMadnessMisery
giftOfMadnessMisery = treachery GiftOfMadnessMisery Cards.giftOfMadnessMisery

instance HasModifiersFor GiftOfMadnessMisery where
  getModifiersFor (GiftOfMadnessMisery a) = case a.placement of
    HiddenInHand iid -> modified_ a iid [CannotTriggerAbilityMatching (#action <> AbilityOnLocation Anywhere)]
    _ -> pure ()

instance HasAbilities GiftOfMadnessMisery where
  getAbilities (GiftOfMadnessMisery a) = [restrictedAbility a 1 InYourHand actionAbility]

instance RunMessage GiftOfMadnessMisery where
  runMessage msg t@(GiftOfMadnessMisery attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ ChooseFrom iid $ chooseRandom attrs MonstersDeck 1
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    ChoseCards _ chosen | isTarget attrs chosen.target -> do
      placeUnderneath ActDeckTarget chosen.cards
      pure t
    _ -> GiftOfMadnessMisery <$> liftRunMessage msg attrs
