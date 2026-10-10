module Arkham.Homebrew.AgesUnwound.Locations.ADisquietingFuture_070 (aDisquietingFuture_070) where

import Arkham.Ability
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ADisquietingFuture_070 = ADisquietingFuture_070 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /A Disquieting Future/, the higher-shroud printing.
aDisquietingFuture_070 :: LocationCard ADisquietingFuture_070
aDisquietingFuture_070 =
  locationWith ADisquietingFuture_070 Cards.aDisquietingFuture_070 5 (PerPlayer 1)
    $ connectsToL
    .~ ringConnections

{- | "Forced - At the end of your turn: Test [intellect] (3). If you succeed,
heal 1 damage or 1 horror. If you fail, take 1 damage and 1 horror."
-}
instance HasAbilities ADisquietingFuture_070 where
  getAbilities (ADisquietingFuture_070 a) =
    extendRevealed1 a $ restricted a endOfTurnAbility Here $ forced $ TurnEnds #when You

instance RunMessage ADisquietingFuture_070 where
  runMessage msg l@(ADisquietingFuture_070 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      canHealDamage <- canHaveDamageHealed (attrs.ability 1) iid
      canHealHorror <- canHaveHorrorHealed (attrs.ability 1) iid
      chooseOrRunOneM iid $ withI18n do
        countVar 1
          $ labeledValidate canHealDamage "healDamage"
          $ healDamage iid (attrs.ability 1) 1
        countVar 1
          $ labeledValidate canHealHorror "healHorror"
          $ healHorror iid (attrs.ability 1) 1
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamageAndHorror iid (attrs.ability 1) 1 1
      pure l
    _ -> ADisquietingFuture_070 <$> liftRunMessage msg attrs
