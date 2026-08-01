module Arkham.Location.Cards.EastAntechamber (eastAntechamber) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Scenarios.CourtOfTheAncients.Helpers
import Arkham.Trait (Trait (Lift))

newtype EastAntechamber = EastAntechamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eastAntechamber :: LocationCard EastAntechamber
eastAntechamber = location EastAntechamber Cards.eastAntechamber 3 (Static 1)

instance HasAbilities EastAntechamber where
  getAbilities (EastAntechamber a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> DuringTurn You <> notExists (LocationWithTrait Lift <> LocationInRow 0))
      $ FastAbility
      $ CostIfLocation
        (be a <> FloodedLocation)
        (GroupClueCost (PerPlayer 2) (be a))
        (GroupClueCost (PerPlayer 1) (be a))

instance RunMessage EastAntechamber where
  runMessage msg l@(EastAntechamber attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      slideGreatLiftDown
      pure l
    _ -> EastAntechamber <$> liftRunMessage msg attrs
