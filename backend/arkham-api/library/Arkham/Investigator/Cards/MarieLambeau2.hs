module Arkham.Investigator.Cards.MarieLambeau2 (marieLambeau2) where

import Arkham.Ability
import Arkham.Draw.Types
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher

newtype MarieLambeau2 = MarieLambeau2 InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

marieLambeau2 :: InvestigatorCard MarieLambeau2
marieLambeau2 =
  investigator MarieLambeau2 Cards.marieLambeau2
    $ Stats {health = 8, sanity = 7, willpower = 4, intellect = 4, combat = 1, agility = 3}

instance HasAbilities MarieLambeau2 where
  getAbilities (MarieLambeau2 attrs) =
    [ playerLimit PerPhase
        $ restricted attrs 1 Self
        $ freeReaction (PlacedCounter #after You AnySource #damage $ atLeast 1)
    ]

instance HasChaosTokenValue MarieLambeau2 where
  getChaosTokenValue iid ElderSign (MarieLambeau2 attrs)
    | iid == attrs.id =
        pure $ ChaosTokenValue ElderSign (PositiveModifier 0)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage MarieLambeau2 where
  runMessage msg i@(MarieLambeau2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsEdit iid (attrs.ability 1) 2 (withCardDrawRule (AfterDrawDiscard 1))
      pure i
    ElderSignEffect iid | iid == attrs.id -> do
      withSkillTest \sid ->
        onSucceedByEffect sid AnyValue (ElderSignEffectSource iid) sid do
          tokenSkillTestOption ElderSign do
            push $ Do $ HealDamage (toTarget iid) (toSource $ ElderSignEffectSource iid) 1
      pure i
    _ -> MarieLambeau2 <$> liftRunMessage msg attrs
