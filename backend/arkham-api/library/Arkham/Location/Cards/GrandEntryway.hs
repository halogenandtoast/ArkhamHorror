module Arkham.Location.Cards.GrandEntryway
  ( grandEntryway
  , GrandEntryway(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GrandEntryway = GrandEntryway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandEntryway :: LocationCard GrandEntryway
grandEntryway = location GrandEntryway Cards.grandEntryway 1 (Static 0)

instance HasAbilities GrandEntryway where
  getAbilities (GrandEntryway attrs) =
    extendRevealed attrs []

instance RunMessage GrandEntryway where
  runMessage msg (GrandEntryway attrs) = runQueueT $ case msg of
    _ -> GrandEntryway <$> liftRunMessage msg attrs
