module Arkham.Homebrew.AgesUnwound.Locations.ArkhamMassachusetts_076 (arkhamMassachusetts_076) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (ControlledAssetsCannotReady))
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ArkhamMassachusetts_076 = ArkhamMassachusetts_076 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /Arkham, Massachusetts (16th Century)/, the printing that costs you upkeep.
arkhamMassachusetts_076 :: LocationCard ArkhamMassachusetts_076
arkhamMassachusetts_076 =
  locationWith ArkhamMassachusetts_076 Cards.arkhamMassachusetts_076 3 (PerPlayer 1)
    $ connectsToL
    .~ ringConnections

{- | "Forced - At the end of your turn: Test [willpower] (2). If you fail, during
the next upkeep phase, your exhausted cards cannot ready."
-}
instance HasAbilities ArkhamMassachusetts_076 where
  getAbilities (ArkhamMassachusetts_076 a) =
    extendRevealed1 a $ restricted a endOfTurnAbility Here $ forced $ TurnEnds #when You

instance RunMessage ArkhamMassachusetts_076 where
  runMessage msg l@(ArkhamMassachusetts_076 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      {- 'nextPhaseModifier #upkeep' is exactly "during the next upkeep phase",
      and ST 4.3 (ready each exhausted card) is the only readying this reaches.
      TODO(ages-unwound): 'ControlledAssetsCannotReady' covers assets, which is
      every card an investigator can have exhausted in this scenario; an
      exhausted *event* in play would still ready. -}
      nextPhaseModifier #upkeep (attrs.ability 1) iid ControlledAssetsCannotReady
      pure l
    _ -> ArkhamMassachusetts_076 <$> liftRunMessage msg attrs
