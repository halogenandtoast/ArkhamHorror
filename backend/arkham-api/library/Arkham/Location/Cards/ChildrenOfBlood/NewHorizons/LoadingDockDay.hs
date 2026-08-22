module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.LoadingDockDay (loadingDockDay) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype LoadingDockDay = LoadingDockDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loadingDockDay :: LocationCard LoadingDockDay
loadingDockDay = symbolLabel $ location LoadingDockDay Cards.loadingDockDay 3 (PerPlayer 1)

instance HasAbilities LoadingDockDay where
  getAbilities (LoadingDockDay a) = extendRevealed a []

instance RunMessage LoadingDockDay where
  runMessage msg (LoadingDockDay attrs) = runQueueT $ LoadingDockDay <$> liftRunMessage msg attrs
