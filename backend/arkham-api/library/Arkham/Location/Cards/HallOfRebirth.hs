module Arkham.Location.Cards.HallOfRebirth (hallOfRebirth, HallOfRebirth (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HallOfRebirth = HallOfRebirth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallOfRebirth :: LocationCard HallOfRebirth
hallOfRebirth = location HallOfRebirth Cards.hallOfRebirth 2 (Static 1)

instance HasAbilities HallOfRebirth where
  getAbilities (HallOfRebirth a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance RunMessage HallOfRebirth where
  runMessage msg l@(HallOfRebirth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCard iid =<< getSetAsideCard Enemies.apostleOfDagon
      pure l
    _ -> HallOfRebirth <$> liftRunMessage msg attrs
