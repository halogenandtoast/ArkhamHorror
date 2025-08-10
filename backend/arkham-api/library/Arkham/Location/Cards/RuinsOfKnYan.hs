module Arkham.Location.Cards.RuinsOfKnYan (ruinsOfKnYan) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RuinsOfKnYan = RuinsOfKnYan LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfKnYan :: LocationCard RuinsOfKnYan
ruinsOfKnYan = location RuinsOfKnYan Cards.ruinsOfKnYan 0 (Static 0)

instance HasAbilities RuinsOfKnYan where
  getAbilities (RuinsOfKnYan attrs) =
    extendRevealed attrs []

instance RunMessage RuinsOfKnYan where
  runMessage msg (RuinsOfKnYan attrs) = runQueueT $ case msg of
    _ -> RuinsOfKnYan <$> liftRunMessage msg attrs
