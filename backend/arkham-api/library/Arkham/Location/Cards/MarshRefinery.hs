module Arkham.Location.Cards.MarshRefinery
  ( marshRefinery
  , MarshRefinery(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MarshRefinery = MarshRefinery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marshRefinery :: LocationCard MarshRefinery
marshRefinery = location MarshRefinery Cards.marshRefinery 1 (PerPlayer 1)

instance HasAbilities MarshRefinery where
  getAbilities (MarshRefinery attrs) =
    extendRevealed attrs []

instance RunMessage MarshRefinery where
  runMessage msg (MarshRefinery attrs) = runQueueT $ case msg of
    _ -> MarshRefinery <$> liftRunMessage msg attrs
