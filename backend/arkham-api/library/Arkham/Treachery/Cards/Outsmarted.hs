module Arkham.Treachery.Cards.Outsmarted (outsmarted) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Outsmarted = Outsmarted TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outsmarted :: TreacheryCard Outsmarted
outsmarted = treachery Outsmarted Cards.outsmarted

instance RunMessage Outsmarted where
  runMessage msg t@(Outsmarted attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Outsmarted <$> liftRunMessage msg attrs
