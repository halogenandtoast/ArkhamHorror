module Arkham.Location.Cards.Garden (garden, Garden (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.ScenarioLogKey

newtype Garden = Garden LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

garden :: LocationCard Garden
garden = location Garden Cards.garden 3 (PerPlayer 1)

instance HasModifiersFor Garden where
  getModifiersFor (Garden a) = whenUnrevealed a $ modifySelf a [Blocked]

instance HasAbilities Garden where
  getAbilities (Garden attrs) =
    extendRevealed1 attrs $ skillTestAbility $ restricted attrs 1 (Here <> NoCluesOnThis) actionAbility

instance RunMessage Garden where
  runMessage msg l@(Garden attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 2)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember DistractedTheGuards
      pure l
    _ -> Garden <$> liftRunMessage msg attrs
