module Arkham.Location.Cards.CrashSite (crashSite, CrashSite (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CrashSite = CrashSite LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crashSite :: LocationCard CrashSite
crashSite = symbolLabel $ location CrashSite Cards.crashSite 2 (Static 0)

instance HasAbilities CrashSite where
  getAbilities (CrashSite attrs) =
    extendRevealed attrs []

instance RunMessage CrashSite where
  runMessage msg (CrashSite attrs) = runQueueT $ case msg of
    _ -> CrashSite <$> liftRunMessage msg attrs
