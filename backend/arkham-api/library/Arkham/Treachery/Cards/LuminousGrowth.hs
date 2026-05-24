module Arkham.Treachery.Cards.LuminousGrowth (luminousGrowth) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LuminousGrowth = LuminousGrowth TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luminousGrowth :: TreacheryCard LuminousGrowth
luminousGrowth = treachery LuminousGrowth Cards.luminousGrowth

instance RunMessage LuminousGrowth where
  runMessage msg t@(LuminousGrowth attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> LuminousGrowth <$> liftRunMessage msg attrs
