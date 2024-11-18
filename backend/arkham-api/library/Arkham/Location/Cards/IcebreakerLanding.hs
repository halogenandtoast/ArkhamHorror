module Arkham.Location.Cards.IcebreakerLanding (icebreakerLanding, IcebreakerLanding (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype IcebreakerLanding = IcebreakerLanding LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

icebreakerLanding :: LocationCard IcebreakerLanding
icebreakerLanding = symbolLabel $ location IcebreakerLanding Cards.icebreakerLanding 0 (Static 0)

instance HasAbilities IcebreakerLanding where
  getAbilities (IcebreakerLanding attrs) =
    extendRevealed attrs []

instance RunMessage IcebreakerLanding where
  runMessage msg (IcebreakerLanding attrs) = runQueueT $ case msg of
    _ -> IcebreakerLanding <$> liftRunMessage msg attrs
