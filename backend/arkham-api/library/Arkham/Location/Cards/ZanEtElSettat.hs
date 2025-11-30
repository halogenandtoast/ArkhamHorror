module Arkham.Location.Cards.ZanEtElSettat (zanEtElSettat) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ZanEtElSettat = ZanEtElSettat LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zanEtElSettat :: LocationCard ZanEtElSettat
zanEtElSettat = symbolLabel $ location ZanEtElSettat Cards.zanEtElSettat 0 (Static 0)

instance HasAbilities ZanEtElSettat where
  getAbilities (ZanEtElSettat a) =
    extendRevealed a []

instance RunMessage ZanEtElSettat where
  runMessage msg (ZanEtElSettat attrs) = runQueueT $ case msg of
    _ -> ZanEtElSettat <$> liftRunMessage msg attrs
