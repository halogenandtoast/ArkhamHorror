module Arkham.Treachery.Cards.LurkingFear (lurkingFear) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LurkingFear = LurkingFear TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lurkingFear :: TreacheryCard LurkingFear
lurkingFear = treachery LurkingFear Cards.lurkingFear

instance RunMessage LurkingFear where
  runMessage msg t@(LurkingFear attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> LurkingFear <$> liftRunMessage msg attrs
