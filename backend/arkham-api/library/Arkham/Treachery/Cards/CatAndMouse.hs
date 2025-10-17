module Arkham.Treachery.Cards.CatAndMouse (catAndMouse) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CatAndMouse = CatAndMouse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catAndMouse :: TreacheryCard CatAndMouse
catAndMouse = treachery CatAndMouse Cards.catAndMouse

instance RunMessage CatAndMouse where
  runMessage msg t@(CatAndMouse attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> CatAndMouse <$> liftRunMessage msg attrs
