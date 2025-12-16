module Arkham.Location.Cards.TheToweringVertexRuinousConflux (theToweringVertexRuinousConflux) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheToweringVertexRuinousConflux = TheToweringVertexRuinousConflux LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theToweringVertexRuinousConflux :: LocationCard TheToweringVertexRuinousConflux
theToweringVertexRuinousConflux = symbolLabel $ location TheToweringVertexRuinousConflux Cards.theToweringVertexRuinousConflux 0 (Static 0)

instance HasAbilities TheToweringVertexRuinousConflux where
  getAbilities (TheToweringVertexRuinousConflux a) =
    extendRevealed a []

instance RunMessage TheToweringVertexRuinousConflux where
  runMessage msg (TheToweringVertexRuinousConflux attrs) = runQueueT $ case msg of
    _ -> TheToweringVertexRuinousConflux <$> liftRunMessage msg attrs
