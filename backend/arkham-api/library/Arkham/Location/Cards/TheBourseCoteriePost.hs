module Arkham.Location.Cards.TheBourseCoteriePost (theBourseCoteriePost) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheBourseCoteriePost = TheBourseCoteriePost LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBourseCoteriePost :: LocationCard TheBourseCoteriePost
theBourseCoteriePost = symbolLabel $ location TheBourseCoteriePost Cards.theBourseCoteriePost 0 (Static 0)

instance HasAbilities TheBourseCoteriePost where
  getAbilities (TheBourseCoteriePost a) =
    extendRevealed a []

instance RunMessage TheBourseCoteriePost where
  runMessage msg (TheBourseCoteriePost attrs) = runQueueT $ case msg of
    _ -> TheBourseCoteriePost <$> liftRunMessage msg attrs
