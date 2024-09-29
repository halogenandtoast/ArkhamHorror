module Arkham.Treachery.Cards.ShapesInTheWater
  ( shapesInTheWater
  , ShapesInTheWater(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ShapesInTheWater = ShapesInTheWater TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shapesInTheWater :: TreacheryCard ShapesInTheWater
shapesInTheWater = treachery ShapesInTheWater Cards.shapesInTheWater

instance RunMessage ShapesInTheWater where
  runMessage msg t@(ShapesInTheWater attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ShapesInTheWater <$> liftRunMessage msg attrs
