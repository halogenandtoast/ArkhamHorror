module Arkham.Treachery.Cards.Confiscation (confiscation) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Matcher

newtype Confiscation = Confiscation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

confiscation :: TreacheryCard Confiscation
confiscation = treachery Confiscation Cards.confiscation

instance RunMessage Confiscation where
  runMessage msg t@(Confiscation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      firearms <- select $ assetControlledBy iid <> #firearm
      for_ firearms (shuffleIntoDeck iid)
      when (null firearms) do
        assignDamage iid attrs 1
        shuffleIntoDeck iid attrs
      pure t
    _ -> Confiscation <$> liftRunMessage msg attrs
