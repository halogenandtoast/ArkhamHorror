module Arkham.Skill.Cards.Resourceful (resourceful, Resourceful (..)) where

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
          focusCards discards \unfocus -> do
            chooseOneM attrs.owner do
              targets cards \card -> do
                push unfocus
                obtainCard card
                addToHand attrs.owner (only card)
      pure s
    _ -> Resourceful <$> liftRunMessage msg attrs
