module Arkham.Location.Cards.TreacherousDescent (treacherousDescent) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TreacherousDescent = TreacherousDescent LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treacherousDescent :: LocationCard TreacherousDescent
treacherousDescent = location TreacherousDescent Cards.treacherousDescent 0 (Static 0)

instance HasAbilities TreacherousDescent where
  getAbilities (TreacherousDescent attrs) =
    extendRevealed attrs []

instance RunMessage TreacherousDescent where
  runMessage msg (TreacherousDescent attrs) = runQueueT $ case msg of
    _ -> TreacherousDescent <$> liftRunMessage msg attrs
