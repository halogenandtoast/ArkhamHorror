module Arkham.Location.Cards.RemnantsOfLakesCamp (remnantsOfLakesCamp, RemnantsOfLakesCamp (..)) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Supplies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype RemnantsOfLakesCamp = RemnantsOfLakesCamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

remnantsOfLakesCamp :: LocationCard RemnantsOfLakesCamp
remnantsOfLakesCamp =
  symbolLabel
    $ locationWith RemnantsOfLakesCamp Cards.remnantsOfLakesCamp 3 (PerPlayer 3)
    $ (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 4) YourLocation)

instance HasAbilities RemnantsOfLakesCamp where
  getAbilities (RemnantsOfLakesCamp a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage RemnantsOfLakesCamp where
  runMessage msg l@(RemnantsOfLakesCamp attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #combat (Fixed 4)
      pure l
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      unlessM (hasSupply WoodenSledge) $ recoverSupply WoodenSledge
      pure l
    _ -> RemnantsOfLakesCamp <$> liftRunMessage msg attrs
