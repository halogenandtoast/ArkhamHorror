module Arkham.Treachery.Cards.Hypothermia (hypothermia) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Hypothermia = Hypothermia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypothermia :: TreacheryCard Hypothermia
hypothermia = treachery Hypothermia Cards.hypothermia

instance RunMessage Hypothermia where
  runMessage msg t@(Hypothermia attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Hypothermia <$> liftRunMessage msg attrs
