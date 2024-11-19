module Arkham.Location.Cards.CrystallineCavern (crystallineCavern, CrystallineCavern (..)) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Supplies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CrystallineCavern = CrystallineCavern LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystallineCavern :: LocationCard CrystallineCavern
crystallineCavern =
  symbolLabel
    $ locationWith CrystallineCavern Cards.crystallineCavern 5 (PerPlayer 3)
    $ (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 4) YourLocation)

instance HasAbilities CrystallineCavern where
  getAbilities (CrystallineCavern a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage CrystallineCavern where
  runMessage msg l@(CrystallineCavern attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 5)
      pure l
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      unlessM (hasSupply MiasmicCrystal) $ recoverSupply MiasmicCrystal
      pure l
    _ -> CrystallineCavern <$> liftRunMessage msg attrs
