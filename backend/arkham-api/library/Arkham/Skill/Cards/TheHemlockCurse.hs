module Arkham.Skill.Cards.TheHemlockCurse (theHemlockCurse) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype TheHemlockCurse = TheHemlockCurse SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHemlockCurse :: SkillCard TheHemlockCurse
theHemlockCurse = skill TheHemlockCurse Cards.theHemlockCurse

instance HasAbilities TheHemlockCurse where
  getAbilities (TheHemlockCurse a) = [restricted a 1 InYourHand $ forced $ TurnEnds #when You]

instance RunMessage TheHemlockCurse where
  runMessage msg s@(TheHemlockCurse attrs) = runQueueT $ case msg of
    FailedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      discardAll iid attrs DiscardableCard
      pure s
    InHand iid (UseCardAbility iid' (isSource attrs -> True) 1 _ _) | iid == iid' -> do
      randomDiscard iid (attrs.ability 1)
      pure s
    _ -> TheHemlockCurse <$> liftRunMessage msg attrs
