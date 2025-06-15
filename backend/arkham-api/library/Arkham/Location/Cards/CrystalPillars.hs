module Arkham.Location.Cards.CrystalPillars (crystalPillars) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CrystalPillars = CrystalPillars LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalPillars :: LocationCard CrystalPillars
crystalPillars = symbolLabel $ location CrystalPillars Cards.crystalPillars 1 (PerPlayer 2)

instance HasAbilities CrystalPillars where
  getAbilities (CrystalPillars a) =
    extendRevealed1 a $ skillTestAbility $ mkAbility a 1 $ forced $ Enters #after You (be a)

instance RunMessage CrystalPillars where
  runMessage msg l@(CrystalPillars attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower
        $ SumCalculation [Fixed 1, VengeanceCalculation]
      pure l
    FailedSkillTest iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ -> do
      assignDamageAndHorror iid (attrs.ability 1) 1 1
      pure l
    _ -> CrystalPillars <$> liftRunMessage msg attrs
