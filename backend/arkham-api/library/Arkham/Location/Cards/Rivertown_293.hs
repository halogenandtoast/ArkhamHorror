module Arkham.Location.Cards.Rivertown_293 (rivertown_293) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.BreachStatus hiding (removeBreaches)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype Rivertown_293 = Rivertown_293 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rivertown_293 :: LocationCard Rivertown_293
rivertown_293 = location Rivertown_293 Cards.rivertown_293 4 (Static 0)

instance HasAbilities Rivertown_293 where
  getAbilities (Rivertown_293 a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage Rivertown_293 where
  runMessage msg l@(Rivertown_293 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
      pure l
    PassedThisSkillTestBy _iid (isAbilitySource attrs 1 -> True) n -> do
      let n' = min (maybe 0 countBreaches $ locationBreaches attrs) n
      act <- selectJust AnyAct
      removeBreaches (toTarget attrs) n'
      placeBreaches (toTarget act) n'
      pure l
    _ -> Rivertown_293 <$> liftRunMessage msg attrs
