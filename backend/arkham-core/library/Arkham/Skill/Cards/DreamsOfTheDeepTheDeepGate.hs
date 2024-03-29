module Arkham.Skill.Cards.DreamsOfTheDeepTheDeepGate (
  dreamsOfTheDeepTheDeepGate,
  DreamsOfTheDeepTheDeepGate (..),
)
where

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype DreamsOfTheDeepTheDeepGate = DreamsOfTheDeepTheDeepGate SkillAttrs
  deriving anyclass (IsSkill)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor DreamsOfTheDeepTheDeepGate where
  getModifiersFor target (DreamsOfTheDeepTheDeepGate attrs) | attrs `is` target = do
    pure $ toModifiers attrs [IfFailureModifier ReturnToHandAfterTest]
  getModifiersFor _ _ = pure []

dreamsOfTheDeepTheDeepGate :: SkillCard DreamsOfTheDeepTheDeepGate
dreamsOfTheDeepTheDeepGate =
  skill DreamsOfTheDeepTheDeepGate Cards.dreamsOfTheDeepTheDeepGate

instance HasAbilities DreamsOfTheDeepTheDeepGate where
  getAbilities (DreamsOfTheDeepTheDeepGate attrs) = [restrictedAbility attrs 1 InYourHand $ forced $ TurnEnds #when You]

instance RunMessage DreamsOfTheDeepTheDeepGate where
  runMessage msg s@(DreamsOfTheDeepTheDeepGate attrs) = case msg of
    InHand iid' (UseThisAbility iid (isSource attrs -> True) 1) | iid' == iid -> do
      pushAll
        [ RevealCard $ toCardId attrs
        , assignDamage iid (CardSource $ toCard attrs) 2
        ]
      pure s
    _ -> DreamsOfTheDeepTheDeepGate <$> runMessage msg attrs
