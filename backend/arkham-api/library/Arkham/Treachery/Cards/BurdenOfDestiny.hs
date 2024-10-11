module Arkham.Treachery.Cards.BurdenOfDestiny (burdenOfDestiny, BurdenOfDestiny (..)) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Trait (Trait (Unbroken))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BurdenOfDestiny = BurdenOfDestiny TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burdenOfDestiny :: TreacheryCard BurdenOfDestiny
burdenOfDestiny = treachery BurdenOfDestiny Cards.burdenOfDestiny

instance RunMessage BurdenOfDestiny where
  runMessage msg t@(BurdenOfDestiny attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      disciplines <- select $ AssetWithTitle "Discipline" <> AssetWithTrait Unbroken
      chooseOrRunOneM iid do
        when (notNull disciplines) do
          labeled "Flip a Discipline you control to its Broken side. It cannot be flipped back this round." do
            chooseOrRunOneM iid do
              targets disciplines \discipline -> do
                flipOverBy iid attrs discipline
                roundModifier attrs discipline CannotBeFlipped
        labeled "Take 1 damage and 1 horror." $ assignDamageAndHorror iid attrs 1 1
      pure t
    _ -> BurdenOfDestiny <$> liftRunMessage msg attrs
