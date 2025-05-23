module Arkham.Treachery.Cards.Paranoia (paranoia) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Paranoia = Paranoia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paranoia :: TreacheryCard Paranoia
paranoia = treachery Paranoia Cards.paranoia

instance RunMessage Paranoia where
  runMessage msg t@(Paranoia attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      loseAllResources iid attrs
      pure t
    _ -> Paranoia <$> liftRunMessage msg attrs
