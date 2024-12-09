module Arkham.Location.Cards.Kitchen (kitchen, Kitchen (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.ScenarioLogKey

newtype Kitchen = Kitchen LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kitchen :: LocationCard Kitchen
kitchen = location Kitchen Cards.kitchen 2 (PerPlayer 1)

instance HasAbilities Kitchen where
  getAbilities (Kitchen attrs) =
    extendRevealed1 attrs $ skillTestAbility $ restricted attrs 1 (Here <> NoCluesOnThis) actionAbility

instance HasModifiersFor Kitchen where
  getModifiersFor (Kitchen a) = whenUnrevealed a $ modifySelf a [Blocked]

instance RunMessage Kitchen where
  runMessage msg l@(Kitchen attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 2)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember SetAFireInTheKitchen
      pure l
    _ -> Kitchen <$> liftRunMessage msg attrs
