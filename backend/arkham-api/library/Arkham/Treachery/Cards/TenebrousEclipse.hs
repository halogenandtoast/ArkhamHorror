module Arkham.Treachery.Cards.TenebrousEclipse (tenebrousEclipse) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TenebrousEclipse = TenebrousEclipse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tenebrousEclipse :: TreacheryCard TenebrousEclipse
tenebrousEclipse = treachery TenebrousEclipse Cards.tenebrousEclipse

instance RunMessage TenebrousEclipse where
  runMessage msg t@(TenebrousEclipse attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> TenebrousEclipse <$> liftRunMessage msg attrs
