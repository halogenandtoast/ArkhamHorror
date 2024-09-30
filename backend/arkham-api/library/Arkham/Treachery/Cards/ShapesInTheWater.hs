module Arkham.Treachery.Cards.ShapesInTheWater (
  shapesInTheWater,
  ShapesInTheWater (..),
)
where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Helpers.Location
import Arkham.Location.FloodLevel
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ShapesInTheWater = ShapesInTheWater TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shapesInTheWater :: TreacheryCard ShapesInTheWater
shapesInTheWater = treachery ShapesInTheWater Cards.shapesInTheWater

instance RunMessage ShapesInTheWater where
  runMessage msg t@(ShapesInTheWater attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      extra <-
        getLocationOf iid >>= \case
          Nothing -> pure 0
          Just loc -> do
            getFloodLevel loc <&> \case
              Unflooded -> 0
              PartiallyFlooded -> 1
              FullyFlooded -> 2
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed $ 2 + extra)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      pure t
    _ -> ShapesInTheWater <$> liftRunMessage msg attrs
