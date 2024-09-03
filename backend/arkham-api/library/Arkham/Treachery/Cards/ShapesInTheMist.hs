module Arkham.Treachery.Cards.ShapesInTheMist (
  shapesInTheMist,
  ShapesInTheMist (..),
) where

import Arkham.Prelude

import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Classes
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ShapesInTheMist = ShapesInTheMist TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shapesInTheMist :: TreacheryCard ShapesInTheMist
shapesInTheMist = treachery ShapesInTheMist Cards.shapesInTheMist

instance RunMessage ShapesInTheMist where
  runMessage msg t@(ShapesInTheMist attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      runHauntedAbilities iid
      pure t
    _ -> ShapesInTheMist <$> runMessage msg attrs
