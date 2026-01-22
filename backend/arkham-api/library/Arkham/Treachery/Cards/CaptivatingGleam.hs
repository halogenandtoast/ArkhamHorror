module Arkham.Treachery.Cards.CaptivatingGleam (captivatingGleam) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CaptivatingGleam = CaptivatingGleam TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

captivatingGleam :: TreacheryCard CaptivatingGleam
captivatingGleam = treachery CaptivatingGleam Cards.captivatingGleam

instance RunMessage CaptivatingGleam where
  runMessage msg t@(CaptivatingGleam attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> CaptivatingGleam <$> liftRunMessage msg attrs
