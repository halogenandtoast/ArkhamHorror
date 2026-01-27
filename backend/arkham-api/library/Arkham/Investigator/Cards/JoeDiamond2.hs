module Arkham.Investigator.Cards.JoeDiamond2 (joeDiamond2) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher

newtype JoeDiamond2 = JoeDiamond2 InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

joeDiamond2 :: InvestigatorCard JoeDiamond2
joeDiamond2 =
  investigator JoeDiamond2 Cards.joeDiamond2
    $ Stats {health = 7, sanity = 7, willpower = 2, intellect = 4, combat = 4, agility = 2}

instance HasAbilities JoeDiamond2 where
  getAbilities (JoeDiamond2 a) =
    [ playerLimit PerRound
        $ selfAbility a 1 (youExist can.draw.cards)
        $ triggered_ (SuccessfulInvestigation #after You Anywhere)
    ]

instance HasChaosTokenValue JoeDiamond2 where
  getChaosTokenValue iid ElderSign (JoeDiamond2 attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage JoeDiamond2 where
  runMessage msg i@(JoeDiamond2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure i
    PassedSkillTestWithToken iid ElderSign | attrs `is` iid -> do
      drawCards iid (attrs.ability 1) 1
      gainResources iid attrs 1
      pure i
    _ -> JoeDiamond2 <$> liftRunMessage msg attrs
