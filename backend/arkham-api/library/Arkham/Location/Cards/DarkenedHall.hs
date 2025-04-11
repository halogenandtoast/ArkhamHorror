module Arkham.Location.Cards.DarkenedHall (darkenedHall) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype DarkenedHall = DarkenedHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkenedHall :: LocationCard DarkenedHall
darkenedHall = symbolLabel $ location DarkenedHall Cards.darkenedHall 4 (Static 0)

instance HasAbilities DarkenedHall where
  getAbilities (DarkenedHall x) =
    extendRevealed1 x $ mkAbility x 1 $ forced $ RevealLocation #after Anyone (be x)

instance RunMessage DarkenedHall where
  runMessage msg l@(DarkenedHall attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeRandomLocationGroupCards "backHallDoorway" [Cards.artGallery, Cards.vipArea, Cards.backAlley]
      pure l
    _ -> DarkenedHall <$> liftRunMessage msg attrs
