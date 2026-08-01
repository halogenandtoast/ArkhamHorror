module Arkham.Location.Cards.WestAntechamber (westAntechamber) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Scenarios.CourtOfTheAncients.Helpers
import Arkham.Trait (Trait (Lift))

newtype WestAntechamber = WestAntechamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westAntechamber :: LocationCard WestAntechamber
westAntechamber = location WestAntechamber Cards.westAntechamber 3 (Static 1)

instance HasAbilities WestAntechamber where
  getAbilities (WestAntechamber a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> DuringTurn You <> notExists (LocationWithTrait Lift <> LocationInRow 0))
      $ FastAbility
      $ CostIfLocation
        (be a <> FloodedLocation)
        (GroupClueCost (PerPlayer 2) (be a))
        (GroupClueCost (PerPlayer 1) (be a))

instance RunMessage WestAntechamber where
  runMessage msg l@(WestAntechamber attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      slideGreatLiftDown
      pure l
    _ -> WestAntechamber <$> liftRunMessage msg attrs
