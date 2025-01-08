module Arkham.Skill.Cards.DreamsOfTheDeepTheDeepGate (dreamsOfTheDeepTheDeepGate) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype DreamsOfTheDeepTheDeepGate = DreamsOfTheDeepTheDeepGate SkillAttrs
  deriving anyclass IsSkill
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor DreamsOfTheDeepTheDeepGate where
  getModifiersFor (DreamsOfTheDeepTheDeepGate attrs) = do
    modifySelf attrs.cardId [IfFailureModifier ReturnToHandAfterTest]

dreamsOfTheDeepTheDeepGate :: SkillCard DreamsOfTheDeepTheDeepGate
dreamsOfTheDeepTheDeepGate =
  skill DreamsOfTheDeepTheDeepGate Cards.dreamsOfTheDeepTheDeepGate

instance HasAbilities DreamsOfTheDeepTheDeepGate where
  getAbilities (DreamsOfTheDeepTheDeepGate attrs) = [restrictedAbility attrs 1 InYourHand $ forced $ TurnEnds #when You]

instance RunMessage DreamsOfTheDeepTheDeepGate where
  runMessage msg s@(DreamsOfTheDeepTheDeepGate attrs) = runQueueT $ case msg of
    InHand iid' (UseThisAbility iid (isSource attrs -> True) 1) | iid' == iid -> do
      push $ RevealCard attrs.cardId
      assignDamage iid attrs.cardId 2
      pure s
    _ -> DreamsOfTheDeepTheDeepGate <$> liftRunMessage msg attrs
