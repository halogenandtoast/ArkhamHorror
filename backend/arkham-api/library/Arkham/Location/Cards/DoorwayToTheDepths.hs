module Arkham.Location.Cards.DoorwayToTheDepths
  ( doorwayToTheDepths
  , DoorwayToTheDepths(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DoorwayToTheDepths = DoorwayToTheDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doorwayToTheDepths :: LocationCard DoorwayToTheDepths
doorwayToTheDepths = location DoorwayToTheDepths Cards.doorwayToTheDepths 5 (Static 1)

instance HasAbilities DoorwayToTheDepths where
  getAbilities (DoorwayToTheDepths attrs) =
    extendRevealed attrs []

instance RunMessage DoorwayToTheDepths where
  runMessage msg (DoorwayToTheDepths attrs) = runQueueT $ case msg of
    _ -> DoorwayToTheDepths <$> liftRunMessage msg attrs
