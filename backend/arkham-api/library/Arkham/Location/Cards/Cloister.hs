module Arkham.Location.Cards.Cloister (cloister, Cloister (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.ScenarioLogKey

newtype Cloister = Cloister LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloister :: LocationCard Cloister
cloister = location Cloister Cards.cloister 2 (PerPlayer 1)

instance HasAbilities Cloister where
  getAbilities (Cloister a) =
    extendRevealed a [skillTestAbility $ restrictedAbility a 1 (Here <> NoCluesOnThis) parleyAction_]

instance RunMessage Cloister where
  runMessage msg l@(Cloister attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember FoundAGuide
      pure l
    _ -> Cloister <$> liftRunMessage msg attrs
