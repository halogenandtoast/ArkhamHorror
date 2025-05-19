module Arkham.Treachery.Cards.GiftOfMadnessPity (giftOfMadnessPity) where

import Arkham.Ability
import Arkham.Choose
import Arkham.Helpers.Modifiers (ModifierType (CannotFight), modified_)
import Arkham.Matcher hiding (PlaceUnderneath, treacheryInHandOf)
import Arkham.Placement
import Arkham.Scenario.Deck
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GiftOfMadnessPity = GiftOfMadnessPity TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

giftOfMadnessPity :: TreacheryCard GiftOfMadnessPity
giftOfMadnessPity = treachery GiftOfMadnessPity Cards.giftOfMadnessPity

instance HasModifiersFor GiftOfMadnessPity where
  getModifiersFor (GiftOfMadnessPity a) = case a.placement of
    HiddenInHand iid -> modified_ a iid [CannotFight (EnemyWithTrait Lunatic)]
    _ -> pure mempty

instance HasAbilities GiftOfMadnessPity where
  getAbilities (GiftOfMadnessPity a) = [restrictedAbility a 1 InYourHand actionAbility]

instance RunMessage GiftOfMadnessPity where
  runMessage msg t@(GiftOfMadnessPity attrs) = runQueueT $ case msg of
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
    _ -> GiftOfMadnessPity <$> liftRunMessage msg attrs
