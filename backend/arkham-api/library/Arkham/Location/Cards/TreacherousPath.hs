module Arkham.Location.Cards.TreacherousPath (treacherousPath, TreacherousPath (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TreacherousPath = TreacherousPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treacherousPath :: LocationCard TreacherousPath
treacherousPath = symbolLabel $ location TreacherousPath Cards.treacherousPath 0 (Static 0)

instance HasAbilities TreacherousPath where
  getAbilities (TreacherousPath attrs) =
    extendRevealed attrs []

instance RunMessage TreacherousPath where
  runMessage msg (TreacherousPath attrs) = runQueueT $ case msg of
    _ -> TreacherousPath <$> liftRunMessage msg attrs
