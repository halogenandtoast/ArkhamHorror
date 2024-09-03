module Arkham.Investigator.Cards.SkidsOTooleParallel (skidsOTooleParallel, SkidsOTooleParallel (..)) where

import Arkham.Ability
import Arkham.Calculation
import Arkham.Helpers.SkillTest (getBaseSkillTestDifficulty, getSkillTest)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.SkillTest.Base
import Arkham.SkillTest.Type

newtype SkidsOTooleParallel = SkidsOTooleParallel InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

skidsOTooleParallel :: InvestigatorCard SkidsOTooleParallel
skidsOTooleParallel =
  investigator SkidsOTooleParallel Cards.skidsOTooleParallel
    $ Stats {health = 8, sanity = 6, willpower = 2, intellect = 3, combat = 3, agility = 4}

instance HasAbilities SkidsOTooleParallel where
  getAbilities (SkidsOTooleParallel a) =
    [playerLimit PerRound $ restrictedAbility a 1 Self $ FastAbility $ UpTo (Fixed 3) (ResourceCost 1)]

instance HasChaosTokenValue SkidsOTooleParallel where
  getChaosTokenValue iid ElderSign (SkidsOTooleParallel attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage SkidsOTooleParallel where
  runMessage msg i@(SkidsOTooleParallel attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalResourcePayment -> n) -> do
      sid <- getRandom
      push
        $ BeginSkillTest
        $ buildSkillTest
          sid
          iid
          (attrs.ability 1)
          attrs
          (BaseValueSkillTest 3 [(#agility, 1), (#wild, 1), (#wildMinus, -1)])
          (FixedBaseValue 3)
          (SkillTestDifficulty $ Fixed n)
      pure i
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      mx <- runMaybeT $ lift . getBaseSkillTestDifficulty =<< MaybeT getSkillTest
      for_ mx \x -> gainResourcesIfCan iid (attrs.ability 1) (2 * x)
      pure i
    ElderSignEffect (is attrs -> True) -> do
      cards <- select $ inDiscardOf attrs.id <> basic (CardWithMaxLevel 2)
      when (notNull cards) $ do
        focusCards cards \unfocus -> do
          chooseOne attrs.id [targetLabel card [unfocus, AddToHand attrs.id [card]] | card <- cards]
      pure i
    _ -> SkidsOTooleParallel <$> liftRunMessage msg attrs
