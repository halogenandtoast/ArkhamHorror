module Arkham.Location.Cards.TreacherousPathDeadlyPass (treacherousPathDeadlyPass) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TreacherousPathDeadlyPass = TreacherousPathDeadlyPass LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treacherousPathDeadlyPass :: LocationCard TreacherousPathDeadlyPass
treacherousPathDeadlyPass = location TreacherousPathDeadlyPass Cards.treacherousPathDeadlyPass 0 (Static 1)

-- TODO: abilities

instance RunMessage TreacherousPathDeadlyPass where
  runMessage msg (TreacherousPathDeadlyPass attrs) = runQueueT $ TreacherousPathDeadlyPass <$> liftRunMessage msg attrs
