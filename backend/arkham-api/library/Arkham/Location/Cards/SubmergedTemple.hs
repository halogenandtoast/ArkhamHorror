module Arkham.Location.Cards.SubmergedTemple
  ( submergedTemple
  , SubmergedTemple(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SubmergedTemple = SubmergedTemple LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

submergedTemple :: LocationCard SubmergedTemple
submergedTemple = location SubmergedTemple Cards.submergedTemple 0 (Static 0)

instance HasAbilities SubmergedTemple where
  getAbilities (SubmergedTemple attrs) =
    extendRevealed attrs []

instance RunMessage SubmergedTemple where
  runMessage msg (SubmergedTemple attrs) = runQueueT $ case msg of
    _ -> SubmergedTemple <$> liftRunMessage msg attrs
