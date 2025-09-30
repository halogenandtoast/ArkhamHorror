module Arkham.Location.Cards.FrenchHill_291 (frenchHill_291) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.BreachStatus hiding (removeBreaches)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype FrenchHill_291 = FrenchHill_291 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frenchHill_291 :: LocationCard FrenchHill_291
frenchHill_291 = location FrenchHill_291 Cards.frenchHill_291 4 (Static 0)

instance HasAbilities FrenchHill_291 where
  getAbilities (FrenchHill_291 a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage FrenchHill_291 where
  runMessage msg l@(FrenchHill_291 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 2)
      pure l
    PassedThisSkillTestBy _iid (isAbilitySource attrs 1 -> True) n -> do
      let n' = min (maybe 0 countBreaches $ locationBreaches attrs) n
      act <- selectJust AnyAct
      removeBreaches attrs n'
      placeBreaches act n'
      pure l
    _ -> FrenchHill_291 <$> liftRunMessage msg attrs
