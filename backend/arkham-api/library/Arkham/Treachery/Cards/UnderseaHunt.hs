module Arkham.Treachery.Cards.UnderseaHunt (underseaHunt) where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (getFloodLevelFor)
import Arkham.Location.FloodLevel (FloodLevel (FullyFlooded))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnderseaHunt = UnderseaHunt TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underseaHunt :: TreacheryCard UnderseaHunt
underseaHunt = treachery UnderseaHunt Cards.underseaHunt

instance RunMessage UnderseaHunt where
  runMessage msg t@(UnderseaHunt attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      fullyFlooded <- (== FullyFlooded) <$> getFloodLevelFor iid
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed $ if fullyFlooded then 5 else 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 1
      locations <- select $ NearestLocationTo iid CanHaveFloodLevelIncreased
      chooseOrRunOneM iid $ targets locations $ push . IncreaseFloodLevel
      pure t
    _ -> UnderseaHunt <$> liftRunMessage msg attrs
