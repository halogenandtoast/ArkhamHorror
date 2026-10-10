module Arkham.Homebrew.AgesUnwound.Treacheries.WeightOfTheCenturies (weightOfTheCenturies) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorActionsPerformed))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype WeightOfTheCenturies = WeightOfTheCenturies TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

weightOfTheCenturies :: TreacheryCard WeightOfTheCenturies
weightOfTheCenturies = treachery WeightOfTheCenturies Cards.weightOfTheCenturies

{- | "Forced - At the end of your turn: Test [willpower] (X), where X is the
number of actions you performed this turn. For each point you failed by, take 1
damage. Discard Weight of the Centuries."
-}
instance HasAbilities WeightOfTheCenturies where
  getAbilities (WeightOfTheCenturies a) =
    [restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You]

instance RunMessage WeightOfTheCenturies where
  runMessage msg t@(WeightOfTheCenturies attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- every action performed this turn, including the ones the "same type of
      -- action" checks ignore; the field hands both lists out together
      n <- length <$> field InvestigatorActionsPerformed iid
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed n)
      pure t
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      assignDamage iid (attrs.ability 1) n
      toDiscard (attrs.ability 1) attrs
      pure t
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> WeightOfTheCenturies <$> liftRunMessage msg attrs
