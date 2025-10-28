module Arkham.Location.Cards.Uptown_297 (uptown_297) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.BreachStatus hiding (removeBreaches)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype Uptown_297 = Uptown_297 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uptown_297 :: LocationCard Uptown_297
uptown_297 = location Uptown_297 Cards.uptown_297 4 (Static 0)

instance HasAbilities Uptown_297 where
  getAbilities (Uptown_297 a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage Uptown_297 where
  runMessage msg l@(Uptown_297 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 2)
      pure l
    PassedThisSkillTestBy _iid (isAbilitySource attrs 1 -> True) n -> do
      let n' = min (maybe 0 countBreaches $ locationBreaches attrs) n
      act <- selectJust AnyAct
      removeBreaches attrs n'
      placeBreaches act n'
      pure l
    _ -> Uptown_297 <$> liftRunMessage msg attrs
