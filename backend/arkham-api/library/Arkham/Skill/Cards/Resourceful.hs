module Arkham.Skill.Cards.Resourceful (resourceful) where

import Arkham.Capability
import Arkham.Card
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Resourceful = Resourceful SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

resourceful :: SkillCard Resourceful
resourceful = skill Resourceful Cards.resourceful

-- Gated on the owner still being able to move cards out of their discard so
-- that a blocker like Graveyard Ghouls can be cleared earlier in ST.7 and this
-- option becomes available again.
returnableCards :: SkillAttrs -> ExtendedCardMatcher
returnableCards attrs =
  InDiscardOf (InvestigatorWithId attrs.owner <> can.have.cards.leaveDiscard)
    <> basic (#survivor <> not_ (CardWithTitle "Resourceful"))

instance RunMessage Resourceful where
  runMessage msg s@(Resourceful attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      skillTestCardOptionEdit attrs (optionWhenExists $ returnableCards attrs) $ doStep 1 msg
      pure s
    DoStep 1 (PassedSkillTest _ _ _ (isTarget attrs -> True) _ _) -> do
      cards <- select $ returnableCards attrs
      discards <- map toCard <$> attrs.owner.discard
      focusCards discards do
        chooseTargetM attrs.owner cards \card -> do
          unfocusCards
          obtainCard card
          addToHand attrs.owner (only card)
      pure s
    _ -> Resourceful <$> liftRunMessage msg attrs
