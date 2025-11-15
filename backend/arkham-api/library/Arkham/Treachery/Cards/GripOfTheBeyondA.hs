module Arkham.Treachery.Cards.GripOfTheBeyondA (gripOfTheBeyondA) where

import Arkham.Ability
import Arkham.Helpers.History
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GripOfTheBeyondA = GripOfTheBeyondA TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gripOfTheBeyondA :: TreacheryCard GripOfTheBeyondA
gripOfTheBeyondA = treachery GripOfTheBeyondA Cards.gripOfTheBeyondA

instance HasAbilities GripOfTheBeyondA where
  getAbilities (GripOfTheBeyondA a) =
    [restricted a 1 InYourThreatArea $ forced $ TurnEnds #when You]

instance RunMessage GripOfTheBeyondA where
  runMessage msg t@(GripOfTheBeyondA attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moved <- getHistoryField TurnHistory iid HistoryMoved
      let n = 2 - moved
      when (n > 0) $ assignDamage iid (attrs.ability 1) n
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> GripOfTheBeyondA <$> liftRunMessage msg attrs
