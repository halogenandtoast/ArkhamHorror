module Arkham.Location.Cards.HoleInTheWall where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards (holeInTheWall)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HoleInTheWall = HoleInTheWall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

holeInTheWall :: LocationCard HoleInTheWall
holeInTheWall = location HoleInTheWall Cards.holeInTheWall 1 (Static 0)

instance HasAbilities HoleInTheWall where
  getAbilities (HoleInTheWall a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage HoleInTheWall where
  runMessage msg l@(HoleInTheWall attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      getSetAsideCardsMatching "Attic" >>= traverse_ placeLocation
      getSetAsideCardsMatching "Cellar" >>= traverse_ placeLocation
      getSetAsideCardsMatching "Parlor" >>= traverse_ placeLocation
      pure l
    _ -> HoleInTheWall <$> liftRunMessage msg attrs
