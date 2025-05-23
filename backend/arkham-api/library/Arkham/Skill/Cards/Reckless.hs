module Arkham.Skill.Cards.Reckless (reckless) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Reckless = Reckless SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reckless :: SkillCard Reckless
reckless = skill Reckless Cards.reckless

instance HasAbilities Reckless where
  getAbilities (Reckless a) = [restricted a 1 InYourHand $ forced $ TurnEnds #when You]

instance RunMessage Reckless where
  runMessage msg s@(Reckless attrs) = runQueueT $ case msg of
    InHand _ (UseCardAbility iid (isSource attrs -> True) 1 _ _) -> do
      revealCard attrs
      loseResources iid (CardIdSource attrs.cardId) 2
      pure s
    FailedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      returnToHand attrs.owner attrs
      pure s
    _ -> Reckless <$> liftRunMessage msg attrs
