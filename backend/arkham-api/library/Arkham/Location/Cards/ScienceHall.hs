module Arkham.Location.Cards.ScienceHall (scienceHall) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Location.Cards qualified as Cards (scienceHall)
import Arkham.Location.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted

newtype ScienceHall = ScienceHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scienceHall :: LocationCard ScienceHall
scienceHall = location ScienceHall Cards.scienceHall 2 (PerPlayer 1)

instance HasAbilities ScienceHall where
  getAbilities (ScienceHall a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ forced
      $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage ScienceHall where
  runMessage msg l@(ScienceHall attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAndDiscardCards iid attrs 1
      pure l
    _ -> ScienceHall <$> liftRunMessage msg attrs
