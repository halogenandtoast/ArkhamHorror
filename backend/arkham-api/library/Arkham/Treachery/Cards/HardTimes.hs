module Arkham.Treachery.Cards.HardTimes (hardTimes, HardTimes (..)) where

import Arkham.Ability
import Arkham.Discard
import Arkham.Helpers.Message.Discard
import Arkham.Helpers.Window (cardsDrawn)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HardTimes = HardTimes TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hardTimes :: TreacheryCard HardTimes
hardTimes = treachery HardTimes Cards.hardTimes

instance HasAbilities HardTimes where
  getAbilities (HardTimes a) =
    [ restrictedAbility a 1 InYourThreatArea $ forced $ DrawsCards #after You (atLeast 1)
    , restrictedAbility a 2 OnSameLocation $ ActionAbility [] $ ActionCost 2
    ]

instance RunMessage HardTimes where
  runMessage msg t@(HardTimes attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (cardsDrawn -> cs) _ -> do
      push $ toMessage $ discardFromHand iid (attrs.ability 1) DiscardChoose (length cs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> HardTimes <$> liftRunMessage msg attrs
