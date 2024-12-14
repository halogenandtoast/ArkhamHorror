module Arkham.Skill.Cards.Daring (daring) where

import Arkham.Helpers.SkillTest
import Arkham.Keyword
import Arkham.Message
import Arkham.Modifier
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

-- Ruling by MJ states the enemy gains to be a lasting effect, while the card
-- draw is not, so we must create a window effect rather than use
-- HasModifiersFor

newtype Daring = Daring SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daring :: SkillCard Daring
daring = skill Daring Cards.daring

instance RunMessage Daring where
  runMessage msg s@(Daring attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill _ sid | sid == attrs.id -> do
      menemy <- ((.enemy) =<<) <$> getSkillTestTarget
      case menemy of
        Just enemy -> do
          withSkillTest \stid -> skillTestModifiers stid attrs enemy [AddKeyword Retaliate, AddKeyword Alert]
        _ -> error "Target was invalid"
      Daring <$> liftRunMessage msg attrs
    SkillTestEnds {} -> do
      drawCardsIfCan (skillOwner attrs) attrs 1
      pure s
    _ -> Daring <$> liftRunMessage msg attrs
