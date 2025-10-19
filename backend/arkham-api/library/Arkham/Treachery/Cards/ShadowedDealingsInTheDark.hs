module Arkham.Treachery.Cards.ShadowedDealingsInTheDark (shadowedDealingsInTheDark) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ShadowedDealingsInTheDark = ShadowedDealingsInTheDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowedDealingsInTheDark :: TreacheryCard ShadowedDealingsInTheDark
shadowedDealingsInTheDark = treachery ShadowedDealingsInTheDark Cards.shadowedDealingsInTheDark

instance RunMessage ShadowedDealingsInTheDark where
  runMessage msg t@(ShadowedDealingsInTheDark attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ShadowedDealingsInTheDark <$> liftRunMessage msg attrs
