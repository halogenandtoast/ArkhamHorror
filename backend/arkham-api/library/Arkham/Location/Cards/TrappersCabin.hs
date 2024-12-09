module Arkham.Location.Cards.TrappersCabin (TrappersCabin (..), trappersCabin) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TrappersCabin = TrappersCabin LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trappersCabin :: LocationCard TrappersCabin
trappersCabin = location TrappersCabin Cards.trappersCabin 3 (Static 0)

instance HasModifiersFor TrappersCabin where
  getModifiersFor (TrappersCabin a) = modifySelect a (investigatorAt a) [CannotGainResources]

instance HasAbilities TrappersCabin where
  getAbilities (TrappersCabin attrs) =
    extendRevealed1 attrs
      $ skillTestAbility
      $ restricted attrs 1 (Here <> notExists (assetIs Assets.bearTrap))
      $ actionAbilityWithCost (ResourceCost 5)

instance RunMessage TrappersCabin where
  runMessage msg l@(TrappersCabin attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      takeControlOfSetAsideAsset iid =<< getSetAsideCard Assets.bearTrap
      pure l
    _ -> TrappersCabin <$> liftRunMessage msg attrs
