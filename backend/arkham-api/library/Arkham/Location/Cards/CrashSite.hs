module Arkham.Location.Cards.CrashSite (crashSite, CrashSite (..)) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Supplies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose

newtype CrashSite = CrashSite LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crashSite :: LocationCard CrashSite
crashSite = symbolLabel $ location CrashSite Cards.crashSite 2 (Static 0)

instance HasAbilities CrashSite where
  getAbilities (CrashSite attrs) =
    extendRevealed attrs [skillTestAbility $ restricted attrs 1 Here actionAbility]

instance RunMessage CrashSite where
  runMessage msg l@(CrashSite attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#intellect, #agility] \sType ->
          skillLabeled sType $ beginSkillTest sid iid (attrs.ability 1) iid sType (Fixed 3)
      pure l
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      unlessM (hasSupply SpareParts) $ recoverSupply SpareParts
      pure l
    _ -> CrashSite <$> liftRunMessage msg attrs
