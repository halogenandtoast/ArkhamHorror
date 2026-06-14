module Arkham.Location.Cards.FleshyPathsEasternBurrows (fleshyPathsEasternBurrows) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FleshyPathsEasternBurrows = FleshyPathsEasternBurrows LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fleshyPathsEasternBurrows :: LocationCard FleshyPathsEasternBurrows
fleshyPathsEasternBurrows = location FleshyPathsEasternBurrows Cards.fleshyPathsEasternBurrows 2 (Static 1)

instance HasAbilities FleshyPathsEasternBurrows where
  getAbilities (FleshyPathsEasternBurrows attrs) =
    extendRevealed1 attrs
      $ skillTestAbility
      $ restricted attrs 1 (Here <> notExists (assetIs Assets.ancientRelic)) actionAbility

instance RunMessage FleshyPathsEasternBurrows where
  runMessage msg l@(FleshyPathsEasternBurrows attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      takeControlOfSetAsideAsset iid =<< getSetAsideCard Assets.ancientRelic
      pure l
    _ -> FleshyPathsEasternBurrows <$> liftRunMessage msg attrs
