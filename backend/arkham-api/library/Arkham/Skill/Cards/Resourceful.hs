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

instance RunMessage Resourceful where
  runMessage msg s@(Resourceful attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ target _ _ | isTarget attrs target -> do
      whenM (can.have.cards.leaveDiscard attrs.owner) do
        cards <- select $ inDiscardOf attrs.owner <> basic (#survivor <> not_ (CardWithTitle "Resourceful"))
        unless (null cards) do
          discards <- map toCard <$> attrs.owner.discard
          skillTestResultOption "Resourceful" do
            focusCards discards do
              chooseTargetM attrs.owner cards \card -> do
                unfocusCards
                obtainCard card
                addToHand attrs.owner (only card)
      pure s
    _ -> Resourceful <$> liftRunMessage msg attrs
