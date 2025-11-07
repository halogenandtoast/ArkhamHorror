module Arkham.Location.Cards.JardinesDeLaTropical (jardinesDeLaTropical) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype JardinesDeLaTropical = JardinesDeLaTropical LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jardinesDeLaTropical :: LocationCard JardinesDeLaTropical
jardinesDeLaTropical = symbolLabel $ location JardinesDeLaTropical Cards.jardinesDeLaTropical 3 (PerPlayer 1)

instance HasAbilities JardinesDeLaTropical where
  getAbilities (JardinesDeLaTropical attrs) =
    extendRevealed attrs []

instance RunMessage JardinesDeLaTropical where
  runMessage msg (JardinesDeLaTropical attrs) = runQueueT $ case msg of
    _ -> JardinesDeLaTropical <$> liftRunMessage msg attrs
