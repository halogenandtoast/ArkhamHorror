module Arkham.Treachery.Cards.GripOfTheBeyondC (gripOfTheBeyondC) where

import Arkham.Ability
import Arkham.Helpers.History
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GripOfTheBeyondC = GripOfTheBeyondC TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gripOfTheBeyondC :: TreacheryCard GripOfTheBeyondC
gripOfTheBeyondC = treachery GripOfTheBeyondC Cards.gripOfTheBeyondC

instance HasAbilities GripOfTheBeyondC where
  getAbilities (GripOfTheBeyondC a) =
    [restricted a 1 InYourThreatArea $ forced $ TurnEnds #when You]

instance RunMessage GripOfTheBeyondC where
  runMessage msg t@(GripOfTheBeyondC attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moved <- getHistoryField TurnHistory iid HistoryMoved
      let n = 2 - moved
      when (n > 0) $ assignDamage iid (attrs.ability 1) n
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> GripOfTheBeyondC <$> liftRunMessage msg attrs
