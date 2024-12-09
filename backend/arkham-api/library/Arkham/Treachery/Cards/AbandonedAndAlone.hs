module Arkham.Treachery.Cards.AbandonedAndAlone (abandonedAndAlone) where

import Arkham.Treachery.Cards qualified as Cards (abandonedAndAlone)
import Arkham.Treachery.Import.Lifted

newtype AbandonedAndAlone = AbandonedAndAlone TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedAndAlone :: TreacheryCard AbandonedAndAlone
abandonedAndAlone = treachery AbandonedAndAlone Cards.abandonedAndAlone

instance RunMessage AbandonedAndAlone where
  runMessage msg t@(AbandonedAndAlone attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      directHorror iid attrs 2
      push $ RemoveDiscardFromGame iid
      pure t
    _ -> AbandonedAndAlone <$> liftRunMessage msg attrs
