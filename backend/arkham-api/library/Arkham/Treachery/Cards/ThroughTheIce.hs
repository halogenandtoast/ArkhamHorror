module Arkham.Treachery.Cards.ThroughTheIce (throughTheIce, ThroughTheIce (..)) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ThroughTheIce = ThroughTheIce TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

throughTheIce :: TreacheryCard ThroughTheIce
throughTheIce = treachery ThroughTheIce Cards.throughTheIce

instance RunMessage ThroughTheIce where
  runMessage msg t@(ThroughTheIce attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ThroughTheIce <$> liftRunMessage msg attrs
