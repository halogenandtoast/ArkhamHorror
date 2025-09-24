module Arkham.Treachery.Cards.ShapesInTheMist (shapesInTheMist) where

import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ShapesInTheMist = ShapesInTheMist TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shapesInTheMist :: TreacheryCard ShapesInTheMist
shapesInTheMist = treachery ShapesInTheMist Cards.shapesInTheMist

instance RunMessage ShapesInTheMist where
  runMessage msg t@(ShapesInTheMist attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      runHauntedAbilities iid
      pure t
    _ -> ShapesInTheMist <$> liftRunMessage msg attrs
