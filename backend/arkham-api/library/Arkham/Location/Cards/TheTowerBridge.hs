module Arkham.Location.Cards.TheTowerBridge (theTowerBridge) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose

newtype TheTowerBridge = TheTowerBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTowerBridge :: LocationCard TheTowerBridge
theTowerBridge = symbolLabel $ location TheTowerBridge Cards.theTowerBridge 2 (PerPlayer 1)

instance HasAbilities TheTowerBridge where
  getAbilities (TheTowerBridge a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage TheTowerBridge where
  runMessage msg l@(TheTowerBridge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#willpower, #intellect] (Fixed 5)
      pure l
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      placeSetAsideLocation_ Cards.traitorsGate
      pure l
    _ -> TheTowerBridge <$> liftRunMessage msg attrs
