module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.LoadingDockNight (loadingDockNight) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype LoadingDockNight = LoadingDockNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loadingDockNight :: LocationCard LoadingDockNight
loadingDockNight = symbolLabel $ location LoadingDockNight Cards.loadingDockNight 3 (PerPlayer 1)

instance HasAbilities LoadingDockNight where
  getAbilities (LoadingDockNight a) = extendRevealed a []

instance RunMessage LoadingDockNight where
  runMessage msg (LoadingDockNight attrs) = runQueueT $ LoadingDockNight <$> liftRunMessage msg attrs
