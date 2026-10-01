module Arkham.Homebrew.CircusExMortis.Locations.MountainStream_167 (mountainStream_167) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MountainStream_167 = MountainStream_167 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mountainStream_167 :: LocationCard MountainStream_167
mountainStream_167 = location MountainStream_167 Cards.mountainStream_167 2 (PerPlayer 1)

instance HasAbilities MountainStream_167 where
  getAbilities (MountainStream_167 a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> youExist (InvestigatorWithClues $ atLeast 1))
      $ forced
      $ TurnEnds #after You

instance RunMessage MountainStream_167 where
  runMessage msg l@(MountainStream_167 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      placeCluesOnLocation iid (attrs.ability 1) 1
      pure l
    _ -> MountainStream_167 <$> liftRunMessage msg attrs
