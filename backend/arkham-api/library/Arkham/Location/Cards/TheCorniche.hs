module Arkham.Location.Cards.TheCorniche (theCorniche) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheCorniche = TheCorniche LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCorniche :: LocationCard TheCorniche
theCorniche = symbolLabel $ location TheCorniche Cards.theCorniche 0 (Static 0)

instance HasAbilities TheCorniche where
  getAbilities (TheCorniche a) =
    extendRevealed a []

instance RunMessage TheCorniche where
  runMessage msg (TheCorniche attrs) = runQueueT $ case msg of
    _ -> TheCorniche <$> liftRunMessage msg attrs
