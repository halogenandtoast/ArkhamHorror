module Arkham.Location.Cards.TheBourseCommercialCenter (theBourseCommercialCenter) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheBourseCommercialCenter = TheBourseCommercialCenter LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBourseCommercialCenter :: LocationCard TheBourseCommercialCenter
theBourseCommercialCenter = symbolLabel $ location TheBourseCommercialCenter Cards.theBourseCommercialCenter 0 (Static 0)

instance HasAbilities TheBourseCommercialCenter where
  getAbilities (TheBourseCommercialCenter a) =
    extendRevealed a []

instance RunMessage TheBourseCommercialCenter where
  runMessage msg (TheBourseCommercialCenter attrs) = runQueueT $ case msg of
    _ -> TheBourseCommercialCenter <$> liftRunMessage msg attrs
