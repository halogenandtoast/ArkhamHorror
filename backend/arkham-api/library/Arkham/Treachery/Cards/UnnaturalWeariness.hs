module Arkham.Treachery.Cards.UnnaturalWeariness (unnaturalWeariness) where

import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnnaturalWeariness = UnnaturalWeariness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unnaturalWeariness :: TreacheryCard UnnaturalWeariness
unnaturalWeariness = treachery UnnaturalWeariness Cards.unnaturalWeariness

instance RunMessage UnnaturalWeariness where
  runMessage msg t@(UnnaturalWeariness attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n | n > 0 -> do
      chooseNM iid (min n 3) $ withI18n do
        countVar 3 $ labeled' "loseResources" $ loseResources iid attrs 3
        countVar 1 $ labeled' "loseActions" $ loseActions iid attrs 1
        labeled' "discardRandomCard" $ randomDiscard iid attrs
      pure t
    _ -> UnnaturalWeariness <$> liftRunMessage msg attrs
