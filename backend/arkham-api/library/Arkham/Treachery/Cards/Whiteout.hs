module Arkham.Treachery.Cards.Whiteout (whiteout, Whiteout (..)) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Whiteout = Whiteout TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whiteout :: TreacheryCard Whiteout
whiteout = treachery Whiteout Cards.whiteout

instance RunMessage Whiteout where
  runMessage msg t@(Whiteout attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Whiteout <$> liftRunMessage msg attrs
