module Arkham.Homebrew.AgesUnwound.Locations.HeartOfAnEmpire_080 (heartOfAnEmpire_080) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HeartOfAnEmpire_080 = HeartOfAnEmpire_080 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /Heart of an Empire/, the printing that merely bleeds you.
heartOfAnEmpire_080 :: LocationCard HeartOfAnEmpire_080
heartOfAnEmpire_080 =
  locationWith HeartOfAnEmpire_080 Cards.heartOfAnEmpire_080 3 (PerPlayer 1)
    $ connectsToL
    .~ ringConnections

-- | "Forced - At the end of your turn: Test [agility] (3). If you fail, take 1 damage."
instance HasAbilities HeartOfAnEmpire_080 where
  getAbilities (HeartOfAnEmpire_080 a) =
    extendRevealed1 a $ restricted a endOfTurnAbility Here $ forced $ TurnEnds #when You

instance RunMessage HeartOfAnEmpire_080 where
  runMessage msg l@(HeartOfAnEmpire_080 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> HeartOfAnEmpire_080 <$> liftRunMessage msg attrs
