module Arkham.Homebrew.AgesUnwound.Locations.ADisquietingFuture_069 (aDisquietingFuture_069) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ADisquietingFuture_069 = ADisquietingFuture_069 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /A Disquieting Future/, the lower-shroud printing. One of a pair; setup
keeps one of the two at random and the other becomes the swap partner agenda 3b
reaches for.
-}
aDisquietingFuture_069 :: LocationCard ADisquietingFuture_069
aDisquietingFuture_069 =
  locationWith ADisquietingFuture_069 Cards.aDisquietingFuture_069 4 (PerPlayer 1)
    $ connectsToL
    .~ ringConnections

{- | "Forced - At the end of your turn: Test [intellect] (3). If you succeed,
draw a card. If you fail, take 1 horror and choose and discard a card from your
hand."
-}
instance HasAbilities ADisquietingFuture_069 where
  getAbilities (ADisquietingFuture_069 a) =
    extendRevealed1 a $ restricted a endOfTurnAbility Here $ forced $ TurnEnds #when You

instance RunMessage ADisquietingFuture_069 where
  runMessage msg l@(ADisquietingFuture_069 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      drawCards iid (attrs.ability 1) 1
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      chooseAndDiscardCard iid (attrs.ability 1)
      pure l
    _ -> ADisquietingFuture_069 <$> liftRunMessage msg attrs
