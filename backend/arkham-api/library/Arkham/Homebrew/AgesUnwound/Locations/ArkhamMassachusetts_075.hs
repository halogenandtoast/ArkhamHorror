module Arkham.Homebrew.AgesUnwound.Locations.ArkhamMassachusetts_075 (arkhamMassachusetts_075) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ArkhamMassachusetts_075 = ArkhamMassachusetts_075 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /Arkham, Massachusetts (16th Century)/ -- the witch-hunt. This is the
starting location when the campaign log says the investigators stepped into the
past, whichever of the two printings setup kept.
-}
arkhamMassachusetts_075 :: LocationCard ArkhamMassachusetts_075
arkhamMassachusetts_075 =
  locationWith ArkhamMassachusetts_075 Cards.arkhamMassachusetts_075 3 (PerPlayer 1)
    $ connectsToL
    .~ ringConnections

{- | "Forced - At the end of your turn: Test [willpower] (3). If you fail, take 1
horror."
-}
instance HasAbilities ArkhamMassachusetts_075 where
  getAbilities (ArkhamMassachusetts_075 a) =
    extendRevealed1 a $ restricted a endOfTurnAbility Here $ forced $ TurnEnds #when You

instance RunMessage ArkhamMassachusetts_075 where
  runMessage msg l@(ArkhamMassachusetts_075 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> ArkhamMassachusetts_075 <$> liftRunMessage msg attrs
